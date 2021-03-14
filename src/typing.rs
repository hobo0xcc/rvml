use crate::env::DeBruijn;
use crate::parse::Node;
use rpds::HashTrieMap;
use std::cell::Cell;
use std::collections::HashMap;
use std::fmt;
use std::process;
use std::{cmp::min, rc::Rc};

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TypeVar {
    Unbound(usize, usize), // (id, level)
    Link(usize),           // id
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Int,
    Float,
    Bool,
    Array(Box<Type>),
    Tuple(Vec<Type>),
    Func { args: Vec<Type>, ret: Box<Type> },
    TVar(Rc<Cell<TypeVar>>), // Type variable
    QVar(usize),             // Quantified type variable
}

impl Type {
    pub fn to_string(&self) -> String {
        match *self {
            Type::Unit => "unit".to_string(),
            Type::Int => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Array(ref ty) => (&**ty).to_string(),
            Type::Tuple(ref types) => {
                let mut ty_name = types.get(0).unwrap().to_string();
                for ty in types.iter().skip(1) {
                    ty_name = format!("{}_{}", ty_name, ty.to_string());
                }
                ty_name
            }
            Type::Func { ref args, ref ret } => {
                let mut ty_name = args.get(0).unwrap().to_string();
                for ty in args.iter().skip(1) {
                    ty_name = format!("{}_{}", ty_name, ty.to_string());
                }
                ty_name = format!("{}_{}", ty_name, (&**ret).to_string());
                ty_name
            }
            Type::TVar(_) | Type::QVar(_) => "var".to_string(),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Type::Unit => write!(f, "unit"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::Array(ref ty) => {
                write!(f, "{} array", ty)
            }
            Type::Tuple(ref t) => {
                for (i, ty) in t.iter().enumerate() {
                    if i == t.len() - 1 {
                        write!(f, "{}", ty)?;
                    } else {
                        write!(f, "{} * ", ty)?;
                    }
                }

                Ok(())
            }
            Type::Func { ref args, ref ret } => {
                write!(f, "(")?;
                for a in args.iter() {
                    write!(f, "{} -> ", a)?;
                }
                write!(f, "{}", **ret)?;
                write!(f, ")")
            }
            Type::TVar(ref tv) => match tv.get() {
                TypeVar::Unbound(id, level) => {
                    write!(f, "t{}/{}", id, level)
                }
                TypeVar::Link(id) => {
                    write!(f, "t{}", id)
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
    Unit,
    Int(i32),
    Float(f64),
    Bool(bool),
    VarExpr(String, Type, Subst),
    VarExtExpr(String, Type),
    Not(Box<TypedNode>),
    Neg(Box<TypedNode>),
    FNeg(Box<TypedNode>),
    Tuple(Vec<TypedNode>, Type),
    Array(Box<TypedNode>, Box<TypedNode>, Type),
    Get(Box<TypedNode>, Box<TypedNode>, Type),
    Put(Box<TypedNode>, Box<TypedNode>, Box<TypedNode>, Type),
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
        name: (String, Type),
        first_expr: Box<TypedNode>,
        second_expr: Box<TypedNode>,
        ty: Type,
    },
    LetTupleExpr {
        names: Vec<(String, Type)>,
        first_expr: Box<TypedNode>,
        second_expr: Box<TypedNode>,
        tuple_ty: Type,
        ty: Type,
    },
    LetRecExpr {
        name: (String, Type),
        args: Vec<(String, Type)>,
        first_expr: Box<TypedNode>,
        second_expr: Box<TypedNode>,
        ty: Type,
    },
    App {
        func: Box<TypedNode>,
        args: Vec<TypedNode>,
        func_ty: Type,
        ty: Type,
    },
}

type Env = DeBruijn<(String, Type)>;
pub type Subst = HashTrieMap<Id, Type>; // DeBruijn<(Id, Type)>;
type Id = usize;

#[derive(Debug, Clone)]
pub enum TypingError {
    UndefinedVar,
    UnknownOp,
    TypeUnmatched,
    SizeUnmatched,
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

    pub fn occurs_check(&self, tvr1: &TypeVar, ty1: &mut Type) -> bool {
        match *ty1 {
            Type::TVar(ref mut tvr2) => {
                if *tvr1 == Rc::clone(tvr2).get() {
                    println!("{:?} : {:?}", tvr1, tvr2.get());
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
                    }
                    TypeVar::Link(ref mut ty2) => {
                        return self.occurs_check(tvr1, &mut self.get_typevar_link(*ty2).unwrap());
                    }
                };
            }
            Type::Array(ref mut ty) => {
                return self.occurs_check(tvr1, ty);
            }
            Type::Tuple(ref mut types) => {
                let mut result = false;
                for ty in types.iter_mut() {
                    result |= self.occurs_check(tvr1, ty);
                }
                return result;
            }
            Type::Func {
                args: ref mut arg_types,
                ret: ref mut ret_type,
            } => {
                let mut result = false;
                for arg_ty in arg_types.iter_mut() {
                    result |= self.occurs_check(tvr1, arg_ty);
                }
                result |= self.occurs_check(tvr1, &mut **ret_type);
                return result;
            }
            _ => return false,
        }
    }

    pub fn unify(&mut self, t1: &mut Type, t2: &mut Type) -> Result<(), TypingError> {
        if *t1 == *t2 {
            return Ok(());
        }
        match (t1, t2) {
            (&mut Type::TVar(ref mut tv), &mut ref mut t_)
            | (&mut ref mut t_, &mut Type::TVar(ref mut tv)) => match Rc::clone(&tv).get() {
                TypeVar::Unbound(ref id, ref _level) => {
                    if self.occurs_check(&Rc::clone(tv).get(), t_) {
                        return Err(TypingError::OccursInside);
                    }
                    tv.set(self.link_typevar(*id, t_.clone()));
                }
                TypeVar::Link(ref mut id) => {
                    let mut linked_ty = self.get_typevar_link(*id).unwrap();
                    self.unify(t_, &mut linked_ty)?;
                }
            },
            (Type::Array(ref mut ty1), Type::Array(ref mut ty2)) => {
                self.unify(ty1, ty2)?;
            }
            (Type::Tuple(ref mut tys1), Type::Tuple(ref mut tys2)) => {
                if tys1.len() != tys2.len() {
                    return Err(TypingError::SizeUnmatched);
                }
                for (ty1, ty2) in tys1.iter_mut().zip(tys2.iter_mut()) {
                    self.unify(ty1, ty2)?;
                }
            }
            (
                Type::Func {
                    args: ref mut args1,
                    ret: ref mut ret1,
                },
                Type::Func {
                    args: ref mut args2,
                    ret: ref mut ret2,
                },
            ) => {
                if args1.len() != args2.len() {
                    println!("{} : {}", args1.len(), args2.len());
                    return Err(TypingError::SizeUnmatched);
                }
                for (a1, a2) in args1.iter_mut().zip(args2.iter_mut()) {
                    self.unify(a1, a2)?;
                }
                self.unify(&mut *ret1, &mut *ret2)?;
            }
            _ => {
                panic!("Error");
                // return Err(TypingError::TypeUnmatched)
            }
        }

        return Ok(());
    }

    pub fn generalize(&self, ty: &Type) -> Type {
        match *ty {
            Type::TVar(ref tv) => match Rc::clone(tv).get() {
                TypeVar::Unbound(ref id, ref level) if *level > self.curr_level => Type::QVar(*id),
                TypeVar::Link(ref id) => {
                    let linked_ty = self.get_typevar_link(*id).unwrap();
                    self.generalize(&linked_ty)
                }
                _ => ty.clone(),
            },
            Type::Array(ref ty) => Type::Array(Box::new(self.generalize(ty))),
            Type::Tuple(ref types) => {
                Type::Tuple(types.iter().map(|ty| self.generalize(ty)).collect())
            }
            Type::Func {
                args: ref arg_types,
                ret: ref ret_type,
            } => Type::Func {
                args: arg_types.iter().map(|ty| self.generalize(ty)).collect(),
                ret: Box::new(self.generalize(&*ret_type)),
            },
            _ => ty.clone(),
        }
    }

    pub fn instantiate_loop(&mut self, subst: Subst, ty: &Type) -> (Type, Subst) {
        match *ty {
            Type::QVar(ref id) => {
                let b = subst.get(id);
                match b {
                    Some(ty) => (ty.clone(), subst),
                    None => {
                        let tv = self.new_typevar();
                        (tv.clone(), subst.insert(*id, tv))
                    }
                }
            }
            Type::TVar(ref tv) => match tv.get() {
                TypeVar::Link(id) => {
                    let ty = self.get_typevar_link(id).unwrap();
                    self.instantiate_loop(subst, &ty)
                }
                _ => (ty.clone(), subst),
            },
            Type::Array(ref ty) => {
                let (ty, subst) = self.instantiate_loop(subst, ty);
                (Type::Array(Box::new(ty)), subst)
            }
            Type::Tuple(ref tys) => {
                let mut subst_tup = subst.clone();
                let mut new_tys = Vec::new();
                for ty in tys.iter() {
                    let (new_ty, new_subst) = self.instantiate_loop(subst_tup, ty);
                    new_tys.push(new_ty);
                    subst_tup = new_subst;
                }

                (Type::Tuple(new_tys), subst_tup)
            }
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

                (
                    Type::Func {
                        args: new_args,
                        ret: Box::new(new_ret),
                    },
                    subst_fn,
                )
            }
            _ => (ty.clone(), subst),
        }
    }

    pub fn instantiate(&mut self, ty: &Type) -> (Type, Subst) {
        self.instantiate_loop(Subst::new(), ty)
    }

    pub fn typing(&mut self, env: Env, node: &Node) -> Result<(TypedNode, Type), TypingError> {
        match *node {
            Node::Unit => Ok((TypedNode::Unit, Type::Unit)),
            Node::Int(ref n) => Ok((TypedNode::Int(*n), Type::Int)),
            Node::Float(ref f) => Ok((TypedNode::Float(*f), Type::Float)),
            Node::Bool(ref b) => Ok((TypedNode::Bool(*b), Type::Bool)),
            Node::VarExpr(ref name) => match env.assoc(name.to_string()) {
                Some(ty) => {
                    let (t, subst) = self.instantiate(&ty);
                    Ok((TypedNode::VarExpr(name.to_string(), t.clone(), subst), t))
                }
                None => {
                    let ty = self.new_typevar();
                    Ok((TypedNode::VarExtExpr(name.to_string(), ty.clone()), ty))
                }
            },
            Node::Not(ref expr) => {
                let (expr_nd, mut expr_ty) = self.typing(env.clone(), &*expr)?;
                self.unify(&mut expr_ty, &mut Type::Bool)?;
                let result_ty = Type::Bool;
                Ok((TypedNode::Not(Box::new(expr_nd)), result_ty))
            }
            Node::Neg(ref expr) => {
                let (expr_nd, mut expr_ty) = self.typing(env.clone(), &*expr)?;
                self.unify(&mut expr_ty, &mut Type::Int)?;
                let result_ty = Type::Int;
                Ok((TypedNode::Neg(Box::new(expr_nd)), result_ty))
            }
            Node::FNeg(ref expr) => {
                let (expr_nd, mut expr_ty) = self.typing(env.clone(), &*expr)?;
                self.unify(&mut expr_ty, &mut Type::Float)?;
                let result_ty = Type::Float;
                Ok((TypedNode::FNeg(Box::new(expr_nd)), result_ty))
            }
            Node::Tuple(ref exprs) => {
                let mut typed_exprs = Vec::new();
                let mut types = Vec::new();
                for expr in exprs.iter() {
                    let (typed_expr, ty) = self.typing(env.clone(), expr)?;
                    typed_exprs.push(typed_expr);
                    types.push(ty);
                }

                let result_ty = Type::Tuple(types);
                Ok((TypedNode::Tuple(typed_exprs, result_ty.clone()), result_ty))
            }
            Node::Array(ref size, ref expr) => {
                let (size_nd, mut size_ty) = self.typing(env.clone(), &**size)?;
                self.unify(&mut size_ty, &mut Type::Int)?;
                let (expr_nd, expr_ty) = self.typing(env.clone(), &**expr)?;
                let array_ty = Type::Array(Box::new(expr_ty));

                Ok((
                    TypedNode::Array(Box::new(size_nd), Box::new(expr_nd), array_ty.clone()),
                    array_ty,
                ))
            }
            Node::Get(ref array, ref idx) => {
                let array_tyvar = self.new_typevar();
                let (array_nd, mut array_ty) = self.typing(env.clone(), &**array)?;
                self.unify(
                    &mut Type::Array(Box::new(array_tyvar.clone())),
                    &mut array_ty,
                )?;
                let (idx_nd, mut idx_ty) = self.typing(env.clone(), &**idx)?;
                self.unify(&mut Type::Int, &mut idx_ty)?;

                Ok((
                    TypedNode::Get(Box::new(array_nd), Box::new(idx_nd), array_tyvar.clone()),
                    array_tyvar,
                ))
            }
            Node::Put(ref array, ref idx, ref new_expr) => {
                let (new_expr_nd, new_expr_ty) = self.typing(env.clone(), &**new_expr)?;
                let (array_nd, mut array_ty) = self.typing(env.clone(), &**array)?;
                let (idx_nd, mut idx_ty) = self.typing(env.clone(), &**idx)?;
                self.unify(&mut Type::Int, &mut idx_ty)?;
                self.unify(
                    &mut Type::Array(Box::new(new_expr_ty.clone())),
                    &mut array_ty,
                )?;

                Ok((
                    TypedNode::Put(
                        Box::new(array_nd),
                        Box::new(idx_nd),
                        Box::new(new_expr_nd),
                        Type::Unit,
                    ),
                    Type::Unit,
                ))
            }
            Node::Expr {
                ref lhs,
                ref op,
                ref rhs,
            } => {
                let mut self_ty = self.new_typevar();
                let (lhs_nd, mut lhs_ty) = self.typing(env.clone(), &*lhs)?;
                let (rhs_nd, mut rhs_ty) = self.typing(env.clone(), &*rhs)?;
                let (mut operand_ty, mut expr_ty) = match op.as_str() {
                    "+" | "-" | "*" | "/" => (Type::Int, Type::Int),
                    "+." | "-." | "*." | "/." => (Type::Float, Type::Float),
                    "<" | ">" | "<=" | ">=" | "<>" | "=" => (self.new_typevar(), Type::Bool),
                    _ => {
                        println!("Unknown op: {}", op);
                        process::exit(1);
                    }
                };
                self.unify(&mut lhs_ty, &mut operand_ty)?;
                self.unify(&mut rhs_ty, &mut operand_ty)?;
                self.unify(&mut self_ty, &mut expr_ty)?;

                let result_ty = self_ty;

                Ok((
                    TypedNode::Expr {
                        lhs: Box::new(lhs_nd),
                        op: op.to_string(),
                        rhs: Box::new(rhs_nd),
                        ty: result_ty.clone(),
                    },
                    result_ty,
                ))
            }
            Node::IfExpr {
                ref cond,
                ref then_body,
                ref else_body,
            } => {
                let mut self_ty = self.new_typevar();
                let (cond_nd, mut cond_ty) = self.typing(env.clone(), cond)?;
                let (then_nd, mut then_ty) = self.typing(env.clone(), then_body)?;
                let (else_nd, mut else_ty) = self.typing(env.clone(), else_body)?;

                self.unify(&mut cond_ty, &mut Type::Bool)?;
                self.unify(&mut then_ty, &mut else_ty)?;
                self.unify(&mut self_ty, &mut then_ty)?;

                let result_ty = self_ty;

                Ok((
                    TypedNode::IfExpr {
                        cond: Box::new(cond_nd),
                        then_body: Box::new(then_nd),
                        else_body: Box::new(else_nd),
                        ty: result_ty.clone(),
                    },
                    result_ty,
                ))
            }
            Node::LetExpr {
                ref name,
                ref first_expr,
                ref second_expr,
            } => {
                self.enter_level();
                let (first_nd, first_ty) = self.typing(env.clone(), &*first_expr)?;
                self.leave_level();

                let new_env = env.clone();
                let (second_nd, second_ty) = self.typing(
                    new_env.add((name.to_string(), first_ty.clone())),
                    &*second_expr,
                )?;

                let result_ty = second_ty;

                Ok((
                    TypedNode::LetExpr {
                        name: (name.to_string(), first_ty),
                        first_expr: Box::new(first_nd),
                        second_expr: Box::new(second_nd),
                        ty: result_ty.clone(),
                    },
                    result_ty,
                ))
            }
            Node::LetTupleExpr {
                ref names,
                ref first_expr,
                ref second_expr,
            } => {
                self.enter_level();
                let (first_nd, mut first_ty) = self.typing(env.clone(), &*first_expr)?;
                self.leave_level();

                let mut tuple_tys = Vec::new();
                for _ in names.iter() {
                    tuple_tys.push(self.new_typevar());
                }
                let mut tuple_ty = Type::Tuple(tuple_tys.clone());

                self.unify(&mut first_ty, &mut tuple_ty)?;

                let mut new_env = env.clone();
                let mut name_tys = Vec::new();
                for (name, ty) in names.iter().zip(tuple_tys.iter()) {
                    new_env = new_env.add((name.to_string(), ty.clone()));
                    name_tys.push((name.to_string(), ty.clone()));
                }
                let (second_nd, second_ty) = self.typing(new_env, &*second_expr)?;

                let result_ty = second_ty;

                Ok((
                    TypedNode::LetTupleExpr {
                        names: name_tys,
                        first_expr: Box::new(first_nd),
                        second_expr: Box::new(second_nd),
                        tuple_ty,
                        ty: result_ty.clone(),
                    },
                    result_ty,
                ))
            }
            Node::LetRecExpr {
                ref name,
                ref args,
                ref first_expr,
                ref second_expr,
            } => {
                self.enter_level();
                let mut fun_tv = self.new_typevar();
                let mut arg_tvs = Vec::new();
                for _name in args.iter() {
                    if _name == "_" {
                        arg_tvs.push(Type::Unit);
                    } else {
                        arg_tvs.push(self.new_typevar());
                    }
                }
                let mut new_env_e1 = env.clone();
                new_env_e1 = new_env_e1.add((name.to_string(), fun_tv.clone()));
                for (arg_tv, name) in arg_tvs.clone().into_iter().zip(args.iter()) {
                    new_env_e1 = new_env_e1.add((name.to_string(), arg_tv));
                }
                let (first_nd, first_ty) = self.typing(new_env_e1, &*first_expr)?;
                self.leave_level();
                self.unify(
                    &mut fun_tv,
                    &mut Type::Func {
                        args: arg_tvs.clone(),
                        ret: Box::new(first_ty),
                    },
                )?;
                let gfun_ty = self.generalize(&fun_tv);
                let new_env_e2 = env.clone();
                let (second_nd, second_ty) = self.typing(
                    new_env_e2.add((name.to_string(), gfun_ty.clone())),
                    &*second_expr,
                )?;

                let result_ty = second_ty;
                Ok((
                    TypedNode::LetRecExpr {
                        name: (name.to_string(), gfun_ty),
                        args: args
                            .into_iter()
                            .map(|s| s.to_string())
                            .zip(arg_tvs.into_iter())
                            .collect(),
                        first_expr: Box::new(first_nd),
                        second_expr: Box::new(second_nd),
                        ty: result_ty.clone(),
                    },
                    result_ty,
                ))
            }
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
                self.unify(
                    &mut fun_ty,
                    &mut Type::Func {
                        args: arg_tys,
                        ret: Box::new(ret_ty.clone()),
                    },
                )?;

                let result_ty = ret_ty;
                Ok((
                    TypedNode::App {
                        func: Box::new(fun_nd),
                        args: arg_nds,
                        func_ty: fun_ty,
                        ty: result_ty.clone(),
                    },
                    result_ty,
                ))
            }
        }
    }

    pub fn deref_ty(&self, ty: &Type) -> Type {
        match *ty {
            Type::Unit => Type::Unit,
            Type::Int => Type::Int,
            Type::Float => Type::Float,
            Type::Bool => Type::Bool,
            Type::Array(ref ty) => Type::Array(Box::new(self.deref_ty(ty))),
            Type::Tuple(ref tys) => {
                let mut new_tys = Vec::new();
                for ty in tys.iter() {
                    let new_ty = self.deref_ty(ty);
                    new_tys.push(new_ty);
                }

                Type::Tuple(new_tys)
            }
            Type::Func { ref args, ref ret } => Type::Func {
                args: args.iter().map(|a| self.deref_ty(a)).collect(),
                ret: Box::new(self.deref_ty(&**ret)),
            },
            Type::TVar(ref tv) => match tv.get() {
                TypeVar::Link(ref id) => self.deref_ty(&self.type_vars.get(id).unwrap().clone()),
                _ => Type::TVar(tv.clone()),
            },
            Type::QVar(ref id) => Type::QVar(*id),
        }
    }

    pub fn deref_node(&self, node: &mut TypedNode) {
        match *node {
            TypedNode::Unit => {}
            TypedNode::Int(_) => {}
            TypedNode::Float(_) => {}
            TypedNode::Bool(_) => {}
            TypedNode::VarExpr(_, ref mut ty, ref mut subst) => {
                *ty = self.deref_ty(ty);
                let subst_cl = subst.clone();
                for (key, val) in subst_cl.iter() {
                    subst.insert_mut(key.clone(), self.deref_ty(val));
                }
            }
            TypedNode::VarExtExpr(_, ref mut ty) => {
                *ty = self.deref_ty(ty);
            }
            TypedNode::Not(ref mut expr) => {
                self.deref_node(&mut **expr);
            }
            TypedNode::Neg(ref mut expr) => {
                self.deref_node(&mut **expr);
            }
            TypedNode::FNeg(ref mut expr) => {
                self.deref_node(&mut **expr);
            }
            TypedNode::Tuple(ref mut tynds, ref mut ty) => {
                *ty = self.deref_ty(ty);
                for tynd in tynds.iter_mut() {
                    self.deref_node(tynd);
                }
            }
            TypedNode::Array(ref mut size, ref mut expr, ref mut ty) => {
                *ty = self.deref_ty(ty);
                self.deref_node(&mut **size);
                self.deref_node(&mut **expr);
            }
            TypedNode::Get(ref mut array, ref mut idx, ref mut ty) => {
                *ty = self.deref_ty(ty);
                self.deref_node(&mut **array);
                self.deref_node(&mut **idx);
            }
            TypedNode::Put(ref mut array, ref mut idx, ref mut new_expr, ref mut ty) => {
                *ty = self.deref_ty(ty);
                self.deref_node(&mut **array);
                self.deref_node(&mut **idx);
                self.deref_node(&mut **new_expr);
            }
            TypedNode::Expr {
                ref mut lhs,
                op: _,
                ref mut rhs,
                ref mut ty,
            } => {
                *ty = self.deref_ty(ty);
                self.deref_node(&mut **lhs);
                self.deref_node(&mut **rhs);
            }
            TypedNode::IfExpr {
                ref mut cond,
                ref mut then_body,
                ref mut else_body,
                ref mut ty,
            } => {
                *ty = self.deref_ty(ty);
                self.deref_node(&mut **cond);
                self.deref_node(&mut **then_body);
                self.deref_node(&mut **else_body);
            }
            TypedNode::LetExpr {
                ref mut name,
                ref mut first_expr,
                ref mut second_expr,
                ref mut ty,
            } => {
                let (_, ref mut name_ty) = name;
                *name_ty = self.deref_ty(name_ty);
                *ty = self.deref_ty(ty);
                self.deref_node(&mut **first_expr);
                self.deref_node(&mut **second_expr);
            }
            TypedNode::LetTupleExpr {
                ref mut names,
                ref mut first_expr,
                ref mut second_expr,
                ref mut tuple_ty,
                ref mut ty,
            } => {
                *ty = self.deref_ty(ty);
                *tuple_ty = self.deref_ty(tuple_ty);
                for (_name, ty) in names.iter_mut() {
                    *ty = self.deref_ty(ty);
                }
                self.deref_node(&mut **first_expr);
                self.deref_node(&mut **second_expr);
            }
            TypedNode::LetRecExpr {
                ref mut name,
                ref mut args,
                ref mut first_expr,
                ref mut second_expr,
                ref mut ty,
            } => {
                let (ref mut _id, ref mut id_ty) = name;
                *ty = self.deref_ty(ty);
                *id_ty = self.deref_ty(id_ty);
                for (_name, a) in args.iter_mut() {
                    *a = self.deref_ty(a);
                }
                self.deref_node(&mut **first_expr);
                self.deref_node(&mut **second_expr);
            }
            TypedNode::App {
                ref mut func,
                ref mut args,
                ref mut func_ty,
                ref mut ty,
            } => {
                *ty = self.deref_ty(ty);
                *func_ty = self.deref_ty(func_ty);
                self.deref_node(&mut **func);
                for a in args.iter_mut() {
                    self.deref_node(a);
                }
            }
        }
    }
}

pub static PRIMITIVES: &'static [&'static str] = &[
    "float_of_int",
    "print_float",
    "print_int",
    "print_newline",
    "truncate",
    "int_of_float",
    "abs_float",
    "sqrt",
    "cos",
    "sin",
    "atan",
    "floor",
    "print_byte",
    "prerr_byte",
    "read_float",
    "read_int",
];

pub fn is_primitive(name: &str) -> bool {
    for prim in PRIMITIVES.iter() {
        if &name == prim {
            return true;
        }
    }

    false
}

pub fn primitive_func(env: Env) -> Env {
    let float_of_int_ty = Type::Func {
        args: vec![Type::Int],
        ret: Box::new(Type::Float),
    };
    let print_float = Type::Func {
        args: vec![Type::Float],
        ret: Box::new(Type::Unit),
    };
    let print_int_ty = Type::Func {
        args: vec![Type::Int],
        ret: Box::new(Type::Unit),
    };
    let print_newline_ty = Type::Func {
        args: vec![Type::Unit],
        ret: Box::new(Type::Unit),
    };
    let truncate_ty = Type::Func {
        args: vec![Type::Float],
        ret: Box::new(Type::Int),
    };
    let int_of_float_ty = Type::Func {
        args: vec![Type::Float],
        ret: Box::new(Type::Int),
    };
    let abs_float_ty = Type::Func {
        args: vec![Type::Float],
        ret: Box::new(Type::Float),
    };
    let sqrt_ty = Type::Func {
        args: vec![Type::Float],
        ret: Box::new(Type::Float),
    };
    let cos_ty = Type::Func {
        args: vec![Type::Float],
        ret: Box::new(Type::Float),
    };
    let sin_ty = Type::Func {
        args: vec![Type::Float],
        ret: Box::new(Type::Float),
    };
    let atan_ty = Type::Func {
        args: vec![Type::Float],
        ret: Box::new(Type::Float),
    };
    let floor_ty = Type::Func {
        args: vec![Type::Float],
        ret: Box::new(Type::Float),
    };
    let print_byte_ty = Type::Func {
        args: vec![Type::Int],
        ret: Box::new(Type::Unit),
    };
    let prerr_byte_ty = Type::Func {
        args: vec![Type::Int],
        ret: Box::new(Type::Unit),
    };
    let read_float_ty = Type::Func {
        args: vec![Type::Unit],
        ret: Box::new(Type::Float),
    };
    let read_int_ty = Type::Func {
        args: vec![Type::Unit],
        ret: Box::new(Type::Int),
    };

    env.add(("float_of_int".to_string(), float_of_int_ty))
        .add(("print_float".to_string(), print_float))
        .add(("print_int".to_string(), print_int_ty))
        .add(("print_newline".to_string(), print_newline_ty))
        .add(("truncate".to_string(), truncate_ty))
        .add(("int_of_float".to_string(), int_of_float_ty))
        .add(("abs_float".to_string(), abs_float_ty))
        .add(("sqrt".to_string(), sqrt_ty))
        .add(("cos".to_string(), cos_ty))
        .add(("sin".to_string(), sin_ty))
        .add(("atan".to_string(), atan_ty))
        .add(("floor".to_string(), floor_ty))
        .add(("print_byte".to_string(), print_byte_ty))
        .add(("prerr_byte".to_string(), prerr_byte_ty))
        .add(("read_float".to_string(), read_float_ty))
        .add(("read_int".to_string(), read_int_ty))
}

pub fn typing(node: Node) -> (TypedNode, Type) {
    println!("Typing");
    let mut t = Typing::new();
    let env = DeBruijn::new();
    match t.typing(primitive_func(env), &node) {
        Ok((mut nd, ref ty)) => {
            t.deref_node(&mut nd);
            (nd, t.deref_ty(ty))
        }
        Err(e) => {
            println!("{:?}", e);
            process::exit(1);
        }
    }
}

#[test]
fn occurs_check_1() {
    let mut t = Typing::new();
    let test_tv = t.new_typevar();
    let tv = match test_tv {
        Type::TVar(ref tv) => tv.clone(),
        _ => unreachable!(),
    };
    {
        let mut test_ty = Type::Func {
            args: vec![test_tv.clone(), Type::Bool],
            ret: Box::new(Type::Int),
        };

        assert!(t.occurs_check(&tv.get(), &mut test_ty));
    }

    {
        let mut test_ty = Type::Func {
            args: vec![Type::Int, Type::Bool],
            ret: Box::new(Type::Int),
        };
        assert!(!t.occurs_check(&tv.get(), &mut test_ty));
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
            ret: Box::new(Type::Int),
        };

        t.unify(&mut test_tv.clone(), &mut Type::Bool).ok();
        let res = t.generalize(&test_ty1);
        assert_eq!(
            res,
            Type::Func {
                args: vec![Type::Bool, Type::Bool],
                ret: Box::new(Type::Int)
            }
        );
    }
}
