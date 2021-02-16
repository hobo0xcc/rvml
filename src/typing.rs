use crate::parse::Node;
use crate::env::{Environment, DeBruijn};
use std::{cmp::min, rc::Rc};
use std::cell::{RefCell, Cell};
use std::collections::HashMap;
use std::process;
use std::fmt;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TypeVar {
    Unbound(usize, usize), // (id, level)
    Link(usize), // id
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Func {
        args: Vec<Type>,
        ret: Box<Type>,
    },
    TVar(Rc<Cell<TypeVar>>), // Type variable
    QVar(usize), // Quantified type variable
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Func { ref args, ref ret } => {
                for a in args.iter() {
                    write!(f, "{} -> ", a)?;
                }
                write!(f, "{}", **ret)
            },
            Type::TVar(ref tv) => {
                match tv.get() {
                    TypeVar::Unbound(id, level) => {
                        write!(f, "t{}/{}", id, level)
                    },
                    TypeVar::Link(id) => {
                        write!(f, "t{}", id)
                    }
                }
            },
            Type::QVar(ref id) => {
                write!(f, "'t{}", id)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypedNode {
    Int(i32),
    Bool(bool),
    VarExpr(String, Type),
    Not(Box<TypedNode>),
    Expr {
        lhs: Box<TypedNode>,
        op: String,
        rhs: Box<TypedNode>,
        ty: Type,
    },
    IfExpr {
        cond: Box<TypedNode>,
        then_body: Box<TypedNode>,
        else_body: Box<TypedNode>,
        ty: Type,
    },
    LetExpr {
        name: String,
        first_expr: Box<TypedNode>,
        second_expr: Box<TypedNode>,
        ty: Type,
    },
    LetRecExpr {
        name: String,
        args: Vec<(String, Type)>,
        first_expr: Box<TypedNode>,
        second_expr: Box<TypedNode>,
        ty: Type,
    },
    App {
        func: Box<TypedNode>,
        args: Vec<TypedNode>,
        ty: Type,
    },
}

type Env = DeBruijn<(String, Type)>;
type Subst = DeBruijn<(Id, Type)>;
type Id = usize;

#[derive(Debug, Clone)]
pub enum TypingError {
    UndefinedVar,
    UnknownOp,
    TypeUnmatched,
    OccursInside,
}

pub struct Typing {
    curr_level: usize,
    next_tv: Id,
    type_vars: HashMap<usize, Type>,
}

impl Typing {
    pub fn new() -> Typing {
        Typing {
            curr_level: 1,
            next_tv: 0,
            type_vars: HashMap::new(),
        }
    }

    pub fn get_typevar_link(&self, tv: Id) -> Option<Type> {
        match self.type_vars.get(&tv) {
            Some(ty) => Some(ty.clone()),
            None => None,
        }
    }

    pub fn link_typevar(&mut self, tv: Id, ty: Type) -> TypeVar {
        self.type_vars.insert(tv, ty);
        return TypeVar::Link(tv);
    }

    pub fn new_typevar(&mut self) -> Type {
        let curr = self.next_tv;
        self.next_tv += 1;
        return Type::TVar(Rc::new(Cell::new(TypeVar::Unbound(curr, self.curr_level))));
    }

    pub fn enter_level(&mut self) {
        self.curr_level += 1;
    }

    pub fn leave_level(&mut self) {
        self.curr_level -= 1;
    }

    pub fn get_vartype(&self, env: Rc<RefCell<Environment<Type>>>, name: &str) -> Option<Type> {
        return (*env).borrow().get(name);
    }

    pub fn put_vartype(&self, env: Rc<RefCell<Environment<Type>>>, name: String, ty: Type) {
        (*env).borrow_mut().set(name, ty);
    }

    pub fn create_env(&self, env: Rc<RefCell<Environment<Type>>>) -> Rc<RefCell<Environment<Type>>> {
        return Rc::new(RefCell::new(Environment::make_child(env)));
    }

    pub fn occurs_check(&self, tvr1: &TypeVar, ty1: &mut Type) -> bool {
        match *ty1 {
            Type::TVar(ref mut tvr2) => {
                if *tvr1 == Rc::clone(tvr2).get() {
                    return true;
                }

                return match Rc::clone(tvr2).get() {
                    TypeVar::Unbound(ref id, ref level) => {
                        let min_level = match *tvr1 {
                            TypeVar::Unbound(_, ref l) => min(*level, *l),
                            _ => *level,
                        };
                        Rc::clone(tvr2).set(TypeVar::Unbound(*id, min_level));
                        false
                    },
                    TypeVar::Link(ref mut ty2) => {
                        return self.occurs_check(tvr1, &mut self.get_typevar_link(*ty2).unwrap());
                    }
                }
            },
            Type::Func { args: ref mut arg_types, ret: ref mut ret_type } => {
                let mut result = false;
                for arg_ty in arg_types.iter_mut() {
                    result &= self.occurs_check(tvr1, arg_ty);
                }
                result &= self.occurs_check(tvr1, &mut **ret_type);
                return result;
            },
            _ => return false,
        }
    }

    pub fn unify(&mut self, t1: &mut Type, t2: &mut Type) -> Result<(), TypingError> {
        if *t1 == *t2 {
            return Ok(());
        }
        match (t1, t2) {
            (&mut Type::TVar(ref mut tv), &mut ref mut t_) | (&mut ref mut t_, &mut Type::TVar(ref mut tv)) => {
                match Rc::clone(&tv).get() {
                    TypeVar::Unbound(ref id, ref _level) => {
                        if self.occurs_check(&Rc::clone(tv).get(), t_) {
                            return Err(TypingError::OccursInside);
                        }
                        tv.set(self.link_typevar(*id, t_.clone()));
                        // tv.replace(TypeVar::Link(Box::new(t_.clone())));
                    },
                    TypeVar::Link(ref mut id) => {
                        let mut linked_ty = self.get_typevar_link(*id).unwrap();
                        self.unify(t_, &mut linked_ty)?;
                    }
                }
            },
            (Type::Func { args: ref mut args1, ret: ref mut ret1 }, Type::Func { args: ref mut args2, ret: ref mut ret2 }) => {
                for (a1, a2) in args1.iter_mut().zip(args2.iter_mut()) {
                    self.unify(a1, a2)?;
                }
                self.unify(&mut *ret1, &mut *ret2)?;
            },
            _ => return Err(TypingError::TypeUnmatched),
        }

        return Ok(());
    }

    pub fn generalize(&self, ty: &Type) -> Type {
        match *ty {
            Type::TVar(ref tv) => {
                match Rc::clone(tv).get() {
                    TypeVar::Unbound(ref id, ref level) if *level > self.curr_level => {
                        Type::QVar(*id)
                    },
                    TypeVar::Link(ref id) => {
                        let linked_ty = self.get_typevar_link(*id).unwrap();
                        self.generalize(&linked_ty)
                    },
                    _ => ty.clone(),
                }
            },
            Type::Func { args: ref arg_types, ret: ref ret_type } => {
                Type::Func {
                    args: arg_types.iter().map(|ty| self.generalize(ty)).collect(),
                    ret: Box::new(self.generalize(&*ret_type)),
                }
            },
            _ => ty.clone()
        }
    }

    pub fn instantiate_loop(&mut self, subst: Subst, ty: &Type) -> (Type, Subst) {
        match *ty {
            Type::QVar(ref id) => {
                let b = subst.assoc(*id);
                match b {
                    Some(ty) => (ty.clone(), subst),
                    None => {
                        let tv = self.new_typevar();
                        (tv.clone(), subst.add((*id, tv)))
                    }
                }
            },
            Type::TVar(ref tv) => {
                match tv.get() {
                    TypeVar::Link(id) => {
                        let ty = self.get_typevar_link(id).unwrap();
                        self.instantiate_loop(subst, &ty)
                    },
                    _ => (ty.clone(), subst),
                }
            },
            Type::Func { ref args, ref ret } => {
                let mut subst_fn = subst.clone();
                let mut new_args = Vec::new();
                for at in args.iter() {
                    let (ty, new_subst) = self.instantiate_loop(subst_fn, at);
                    new_args.push(ty);
                    subst_fn = new_subst;
                }
                let (new_ret, new_subst) = self.instantiate_loop(subst_fn, &*ret);
                subst_fn = new_subst;
                
                (Type::Func {
                    args: new_args,
                    ret: Box::new(new_ret),
                }, subst_fn)
            },
            _ => (ty.clone(), subst)
        }
    }

    pub fn instantiate(&mut self, ty: &Type) -> Type {
        self.instantiate_loop(Subst::new(), ty).0
    }

    pub fn typing(&mut self, env: Env, node: &Node) -> Result<(TypedNode, Type), TypingError> {
        match *node {
            Node::Int(ref n) => Ok((TypedNode::Int(*n), Type::Int)),
            Node::Bool(ref b) => Ok((TypedNode::Bool(*b), Type::Bool)),
            Node::VarExpr(ref name) => {
                match env.assoc(name.to_string()) {
                    Some(ty) => {
                        let t = self.instantiate(&ty);
                        Ok((TypedNode::VarExpr(name.to_string(), t.clone()), t))
                    },
                    None => Err(TypingError::UndefinedVar),
                }
            },
            Node::Not(ref expr) => {
                let (expr_nd, mut expr_ty) = self.typing(env.clone(), &*expr)?;
                self.unify(&mut expr_ty, &mut Type::Bool)?;
                let result_ty = Type::Bool;
                Ok((TypedNode::Not(
                    Box::new(expr_nd),
                ), result_ty))
            },
            Node::Expr { ref lhs, ref op, ref rhs } => {
                let mut self_ty = self.new_typevar();
                let (lhs_nd, mut lhs_ty) = self.typing(env.clone(), &*lhs)?;
                let (rhs_nd, mut rhs_ty) = self.typing(env.clone(), &*rhs)?;
                let (mut operand_ty, mut expr_ty) = match op.as_str() {
                    "+" | "-" | "*" | "/" => (Type::Int, Type::Int),
                    "<=" | "=" => (Type::Int, Type::Bool),
                    _ => unreachable!(),
                };
                self.unify(&mut lhs_ty, &mut operand_ty)?;
                self.unify(&mut rhs_ty, &mut operand_ty)?;
                self.unify(&mut self_ty, &mut expr_ty)?;

                let result_ty = self_ty;

                Ok((TypedNode::Expr {
                    lhs: Box::new(lhs_nd),
                    op: op.to_string(),
                    rhs: Box::new(rhs_nd),
                    ty: result_ty.clone(),
                }, result_ty))
            },
            Node::IfExpr { ref cond, ref then_body, ref else_body } => {
                let mut self_ty = self.new_typevar();
                let (cond_nd, mut cond_ty) = self.typing(env.clone(), cond)?;
                let (then_nd, mut then_ty) = self.typing(env.clone(), then_body)?;
                let (else_nd, mut else_ty) = self.typing(env.clone(), else_body)?;

                self.unify(&mut cond_ty, &mut Type::Bool)?;
                self.unify(&mut then_ty, &mut else_ty)?;
                self.unify(&mut self_ty, &mut then_ty)?;

                let result_ty = self_ty;

                Ok((TypedNode::IfExpr {
                    cond: Box::new(cond_nd),
                    then_body: Box::new(then_nd),
                    else_body: Box::new(else_nd),
                    ty: result_ty.clone(),
                }, result_ty))
            },
            Node::LetExpr { ref name, ref first_expr, ref second_expr } => {
                self.enter_level();
                let (first_nd, first_ty) = self.typing(env.clone(), &*first_expr)?;
                self.leave_level();

                let new_env = env.clone();
                let (second_nd, second_ty) = self.typing(new_env.add((name.to_string(), first_ty)), &*second_expr)?;

                let result_ty = second_ty;

                Ok((TypedNode::LetExpr {
                    name: name.to_string(),
                    first_expr: Box::new(first_nd),
                    second_expr: Box::new(second_nd),
                    ty: result_ty.clone(),
                }, result_ty))
            },
            Node::LetRecExpr { ref name, ref args, ref first_expr, ref second_expr } => {
                self.enter_level();
                let mut fun_tv = self.new_typevar();
                let mut arg_tvs = Vec::new();
                for _ in args.iter() {
                    arg_tvs.push(self.new_typevar());
                }
                let mut new_env_e1 = env.clone();
                new_env_e1 = new_env_e1.add((name.to_string(), fun_tv.clone()));
                for (arg_tv, name) in arg_tvs.clone().into_iter().zip(args.iter()) {
                    new_env_e1 = new_env_e1.add((name.to_string(), arg_tv));
                }
                let (first_nd, first_ty) = self.typing(new_env_e1, &*first_expr)?;
                self.leave_level();
                self.unify(&mut fun_tv, &mut Type::Func { args: arg_tvs.clone(), ret: Box::new(first_ty) })?;
                let gfun_ty = self.generalize(&fun_tv);
                let new_env_e2 = env.clone();
                let (second_nd, second_ty) = self.typing(new_env_e2.add((name.to_string(), gfun_ty)), &*second_expr)?;
                
                let result_ty = second_ty;
                Ok((TypedNode::LetRecExpr {
                    name: name.to_string(),
                    args: args.into_iter().map(|s| s.to_string()).zip(arg_tvs.into_iter()).collect(),
                    first_expr: Box::new(first_nd),
                    second_expr: Box::new(second_nd),
                    ty: result_ty.clone(),
                }, result_ty))
            },
            Node::App { ref func, ref args } => {
                let (fun_nd, mut fun_ty) = self.typing(env.clone(), &*func)?;
                let mut arg_tys = Vec::new();
                let mut arg_nds = Vec::new();
                for a in args.iter() {
                    let (arg_nd, arg_ty) = self.typing(env.clone(), a)?;
                    arg_tys.push(arg_ty);
                    arg_nds.push(arg_nd)
                }

                let ret_ty = self.new_typevar();
                self.unify(&mut fun_ty, &mut Type::Func { args: arg_tys, ret: Box::new(ret_ty.clone()) })?;

                let result_ty = ret_ty;
                Ok((TypedNode::App {
                    func: Box::new(fun_nd),
                    args: arg_nds,
                    ty: result_ty.clone(),
                }, result_ty))
            }
        }
    }

    pub fn deref_ty(&self, ty: &Type) -> Type {
        match *ty {
            Type::Int => Type::Int,
            Type::Bool => Type::Bool,
            Type::Func { ref args, ref ret } => {
                Type::Func {
                    args: args.iter().map(|a| self.deref_ty(a)).collect(),
                    ret: Box::new(self.deref_ty(&**ret)),
                }
            },
            Type::TVar(ref tv) => {
                match tv.get() {
                    TypeVar::Link(ref id) => {
                        self.deref_ty(&self.type_vars.get(id).unwrap().clone())
                    },
                    _ => Type::TVar(tv.clone()),
                }
            },
            Type::QVar(ref id) => Type::QVar(*id),
        }
    }

    pub fn deref_node(&self, node: &mut TypedNode) {
        match *node {
            TypedNode::Int(_) => {},
            TypedNode::Bool(_) => {},
            TypedNode::VarExpr(_, ref mut ty) => {
                *ty = self.deref_ty(ty);
            },
            TypedNode::Not(ref mut expr) => {
                self.deref_node(&mut **expr);
            },
            TypedNode::Expr { ref mut lhs, op: _, ref mut rhs, ref mut ty } => {
                *ty = self.deref_ty(ty);
                self.deref_node(&mut **lhs);
                self.deref_node(&mut **rhs);
            },
            TypedNode::IfExpr { ref mut cond, ref mut then_body, ref mut else_body, ref mut ty } => {
                *ty = self.deref_ty(ty);
                self.deref_node(&mut **cond);
                self.deref_node(&mut **then_body);
                self.deref_node(&mut **else_body);
            },
            TypedNode::LetExpr { name: _, ref mut first_expr, ref mut second_expr, ref mut ty } => {
                *ty = self.deref_ty(ty);
                self.deref_node(&mut **first_expr);
                self.deref_node(&mut **second_expr);
            },
            TypedNode::LetRecExpr { name: _, ref mut args, ref mut first_expr, ref mut second_expr, ref mut ty } => {
                *ty = self.deref_ty(ty);
                for (_name, a) in args.iter_mut() {
                    *a = self.deref_ty(a);
                }
                self.deref_node(&mut **first_expr);
                self.deref_node(&mut **second_expr);
            },
            TypedNode::App { ref mut func, ref mut args, ref mut ty } => {
                *ty = self.deref_ty(ty);
                self.deref_node(&mut **func);
                for a in args.iter_mut() {
                    self.deref_node(a);
                }
            }
        }
    }
}

pub fn typing(node: Node) -> (TypedNode, Type) {
    let mut t = Typing::new();
    match t.typing(DeBruijn::new(), &node) {
        Ok((mut nd, ref ty)) => {
            t.deref_node(&mut nd);
            (nd, t.deref_ty(ty))
        },
        Err(e) => {
            println!("{:?}", e);
            process::exit(1);
        },
    }
}

#[test]
fn occurs_check_1() {
    let mut t = Typing::new();
    let test_tv = t.new_typevar();
    let tv = match test_tv { Type::TVar(ref tv) => tv.clone(), _ => unreachable!(), };
    {
        let mut test_ty = Type::Func {
            args: vec![test_tv.clone(), Type::Bool],
            ret: Box::new(Type::Int),
        };

        assert!(t.occurs_check(&tv.get(), &mut test_ty) == false);
    }

    {
        let mut test_ty = Type::Func {
            args: vec![Type::Int, Type::Bool],
            ret: Box::new(Type::Int),
        };
        assert!(t.occurs_check(&tv.get(), &mut test_ty) == false);
    }
}

#[test]
fn unify_1() {
    let mut t = Typing::new();
    let test_tv = t.new_typevar();
    {
        let mut test_ty1 = Type::Bool;
        let mut test_ty2 = test_tv.clone();
        
        let result = t.unify(&mut test_ty1, &mut test_ty2);
        assert!(result.is_ok());
    }
}

#[test]
fn generalize_1() {
    let mut t = Typing::new();
    let test_tv = t.new_typevar();
    {
        let test_ty1 = Type::Func {
            args: vec![test_tv.clone(), Type::Bool],
            ret: Box::new(Type::Int)
        };
        
        t.unify(&mut test_tv.clone(), &mut Type::Bool).ok();
        let res = t.generalize(&test_ty1);
        assert_eq!(res, Type::Func { args: vec![Type::Bool, Type::Bool], ret: Box::new(Type::Int) });
    }
}

