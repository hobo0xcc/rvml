use rpds::{HashTrieMap, HashTrieSet};
use crate::typing::{Type, TypedNode};
use std::hash::Hash;

#[derive(Clone, Debug, PartialEq)]
pub enum CNode {
    Int(i32),
    Bool(bool),
    VarExpr(String, Type),
    Not(Box<CNode>),
    Tuple(Vec<CNode>, Type),
    Expr {
        lhs: Box<CNode>,
        op: String,
        rhs: Box<CNode>,
        ty: Type,
    },
    IfExpr {
        cond: Box<CNode>,
        then_body: Box<CNode>,
        else_body: Box<CNode>,
        ty: Type,
    },
    LetExpr {
        name: (String, Type),
        first_expr: Box<CNode>,
        second_expr: Box<CNode>,
        ty: Type,
    },
    LetTupleExpr {
        names: Vec<(String, Type)>,
        first_expr: Box<CNode>,
        second_expr: Box<CNode>,
        tuple_ty: Type,
        ty: Type,
    },
    MakeCls {
        name: (String, Type),
        actual_fv: Vec<String>,
        second_expr: Box<CNode>,
        ty: Type,
    },
    AppCls {
        func: Box<CNode>,
        args: Vec<CNode>,
        func_ty: Type,
        ty: Type,
    },
    AppDir {
        func: String,
        args: Vec<CNode>,
        func_ty: Type,
        ty: Type,
    }
}

type Env = HashTrieMap<String, Type>;
type Known = HashTrieSet<String>;

#[derive(Debug, Clone)]
pub struct FunDef {
    pub name: (String, Type),
    pub args: Vec<(String, Type)>,
    pub formal_fv: Vec<(String, Type)>,
    pub body: CNode,
}

pub struct Closure {
    toplevel: Vec<FunDef>,
}

impl Closure {
    pub fn new() -> Closure {
        Closure {
            toplevel: Vec::new(),
        }
    }

    pub fn union<T>(s1: HashTrieSet<T>, s2: HashTrieSet<T>) -> HashTrieSet<T>
    where T: Eq + Hash + Clone {
        let mut new_s = HashTrieSet::new();
        for e in s1.into_iter() {
            new_s = new_s.insert(e.clone());
        }
        for e in s2.into_iter() {
            new_s = new_s.insert(e.clone());
        }

        new_s
    }

    pub fn inter<T>(s1: HashTrieSet<T>, s2: HashTrieSet<T>) -> HashTrieSet<T>
    where T: Eq + Hash + Clone {
        let mut new_s = HashTrieSet::new();
        for e in s1.into_iter() {
            if s2.contains(e) {
                new_s = new_s.insert(e.clone());
            }
        }

        new_s
    }

    pub fn diff<T>(s1: HashTrieSet<T>, s2: HashTrieSet<T>) -> HashTrieSet<T>
    where T: Eq + Hash + Clone {
        let mut new_s = HashTrieSet::new();
        for e in s1.into_iter() {
            if !s2.contains(e) {
                new_s = new_s.insert(e.clone());
            }
        }

        new_s
    }

    pub fn fv(&mut self, node: &CNode) -> Known {
        match *node {
            CNode::Int(_) | CNode::Bool(_) => Known::new(),
            CNode::VarExpr(ref name, ref _ty) => {
                Known::new().insert(name.to_string())
            },
            CNode::Not(ref expr) => {
                self.fv(&**expr)
            },
            CNode::Tuple(ref nds, ref _ty) => {
                let mut fvs = Known::new();
                for nd in nds.iter() {
                    fvs = Closure::union(fvs, self.fv(nd));
                }

                fvs
            },
            CNode::Expr {
                ref lhs,
                op: ref _op,
                ref rhs,
                ty: ref _ty
            } => {
                let fvs = Closure::union(self.fv(&**lhs), self.fv(&**rhs));

                fvs
            },
            CNode::IfExpr {
                ref cond,
                ref then_body,
                ref else_body,
                ty: ref _ty,
            } => {
                let mut fvs = Closure::union(self.fv(&**cond), self.fv(&**then_body));
                fvs = Closure::union(fvs, self.fv(&**else_body));

                fvs
            },
            CNode::LetExpr {
                ref name,
                ref first_expr,
                ref second_expr,
                ty: ref _ty
            } => {
                let (ref id, _) = name;
                Closure::union(self.fv(&**first_expr), self.fv(&**second_expr).remove(id))
            },
            CNode::LetTupleExpr {
                ref names,
                ref first_expr,
                ref second_expr,
                tuple_ty: ref _tuple_ty,
                ty: ref _ty,
            } => {
                let fv_e1 = self.fv(&**first_expr);
                let mut fv_e2 = self.fv(&**second_expr);

                for (id, _) in names.iter() {
                    fv_e2 = fv_e2.remove(id);
                }

                Closure::union(fv_e1, fv_e2)
            },
            CNode::MakeCls {
                ref name,
                ref actual_fv,
                ref second_expr,
                ty: ref _ty,
            } => {
                let mut fvs = Known::new();
                for fv in actual_fv.iter() {
                    fvs = fvs.insert(fv.to_string());
                }
                fvs = Closure::union(fvs, self.fv(&**second_expr));
                let (ref id, ref _id_ty) = name;
                fvs = fvs.remove(id);

                fvs
            },
            CNode::AppCls {
                ref func,
                ref args,
                func_ty: ref _func_ty,
                ty: ref _ty,
            } => {
                let mut fvs = self.fv(&**func);
                for arg in args.iter() {
                    fvs = Closure::union(fvs, self.fv(arg));
                }

                fvs
            },
            CNode::AppDir {
                func: ref _func,
                ref args,
                func_ty: ref _func_ty,
                ty: ref _ty,
            } => {
                let mut fvs = Known::new();
                for arg in args.iter() {
                    fvs = Closure::union(fvs, self.fv(arg));
                }

                fvs
            }
        }
    }

    pub fn closure(&mut self, env: Env, known: Known, node: &TypedNode) -> CNode {
        use crate::typing::TypedNode::*;
        match *node {
            Int(ref n) => CNode::Int(*n),
            Bool(ref b) => CNode::Bool(*b),
            VarExpr(ref name, ref ty) => CNode::VarExpr(name.to_string(), ty.clone()),
            Not(ref expr) => CNode::Not(Box::new(self.closure(env, known, &**expr))),
            Tuple(ref nds, ref ty) => {
                let mut new_nds = Vec::new();
                for nd in nds.iter() {
                    new_nds.push(self.closure(env.clone(), known.clone(), nd));
                }

                CNode::Tuple(new_nds, ty.clone())
            },
            Expr {
                ref lhs,
                ref op,
                ref rhs,
                ref ty,
            } => {
                let new_lhs = self.closure(env.clone(), known.clone(), &**lhs);
                let new_rhs = self.closure(env.clone(), known.clone(), &**rhs);

                CNode::Expr {
                    lhs: Box::new(new_lhs),
                    op: op.to_string(),
                    rhs: Box::new(new_rhs),
                    ty: ty.clone(),
                }
            },
            IfExpr {
                ref cond,
                ref then_body,
                ref else_body,
                ref ty,
            } => {
                let new_cond = self.closure(env.clone(), known.clone(), &**cond);
                let new_then_body = self.closure(env.clone(), known.clone(), &**then_body);
                let new_else_body = self.closure(env.clone(), known.clone(), &**else_body);
                
                CNode::IfExpr {
                    cond: Box::new(new_cond),
                    then_body: Box::new(new_then_body),
                    else_body: Box::new(new_else_body),
                    ty: ty.clone(),
                }
            },
            LetExpr {
                ref name,
                ref first_expr,
                ref second_expr,
                ref ty,
            } => {
                let new_first_expr = self.closure(env.clone(), known.clone(), &**first_expr);
                let (ref id, ref id_ty) = name;
                let new_second_expr = self.closure(env.insert(id.to_string(), id_ty.clone()), known.clone(), &**second_expr);
                
                CNode::LetExpr {
                    name: (id.to_string(), id_ty.clone()),
                    first_expr: Box::new(new_first_expr),
                    second_expr: Box::new(new_second_expr),
                    ty: ty.clone(),
                }
            },
            LetTupleExpr {
                ref names,
                ref first_expr,
                ref second_expr,
                ref tuple_ty,
                ref ty,
            } => {
                let new_first_expr = self.closure(env.clone(), known.clone(), &**first_expr);
                let mut new_env = env.clone();
                for (id, id_ty) in names.iter() {
                    new_env = new_env.insert(id.to_string(), id_ty.clone());
                }
                let new_second_expr = self.closure(new_env, known.clone(), &**second_expr);

                CNode::LetTupleExpr {
                    names: names.clone(),
                    first_expr: Box::new(new_first_expr),
                    second_expr: Box::new(new_second_expr),
                    tuple_ty: tuple_ty.clone(),
                    ty: ty.clone(),
                }
            },
            LetRecExpr {
                ref name,
                ref args,
                ref first_expr,
                ref second_expr,
                ref ty,
            } => {
                let toplevel_backup = self.toplevel.clone();
                let (ref id, ref id_ty) = name;
                let new_env = env.insert(id.to_string(), id_ty.clone());
                let new_known = known.insert(id.to_string());

                let mut env_e1 = new_env.clone();
                for (arg_id, arg_ty) in args.iter() {
                    env_e1 = env_e1.insert(arg_id.to_string(), arg_ty.clone());
                }
                let e1 = self.closure(env_e1, new_known.clone(), &**first_expr);
                let mut s_args = Known::new();
                for (arg_id, _) in args.iter() {
                    s_args = s_args.insert(arg_id.to_string());
                }

                let zs = Closure::diff(self.fv(&e1), s_args);
                let (new_known, e1) = if zs.is_empty() {
                    (new_known, e1)
                } else {
                    self.toplevel = toplevel_backup;
                    let mut env_e1 = new_env.clone();
                    for (arg_id, arg_ty) in args.iter() {
                        env_e1 = env_e1.insert(arg_id.to_string(), arg_ty.clone());
                    }
                    let e1 = self.closure(env_e1, known.clone(), first_expr);
                    (known, e1)
                };

                let mut s_args = Known::new();
                for (arg_id, _) in args.iter() {
                    s_args = s_args.insert(arg_id.to_string());
                }
                s_args = s_args.insert(id.to_string());

                let zs = Closure::diff(self.fv(&e1), s_args);
                let zts = zs.iter().map(|x| (x.to_string(), new_env.get(x).unwrap().clone())).collect();
                let new_fun = FunDef {
                    name: (id.to_string(), id_ty.clone()),
                    args: args.clone(),
                    formal_fv: zts,
                    body: e1,
                };
                self.toplevel.push(new_fun);
                let e2 = self.closure(new_env, new_known, &**second_expr);
                if self.fv(&e2).contains(id) {
                    CNode::MakeCls {
                        name: (id.to_string(), id_ty.clone()),
                        actual_fv: zs.iter().map(|x| x.to_string()).collect(),
                        second_expr: Box::new(e2),
                        ty: ty.clone(),
                    }
                } else {
                    e2
                }
            },
            App {
                ref func,
                ref args,
                ref func_ty,
                ref ty
            } => {
                let new_func = self.closure(env.clone(), known.clone(), &**func);
                let is_known;
                let mut label_name: String = String::new();
                match new_func {
                    CNode::VarExpr(ref name, ref _ty) => {
                        is_known = known.contains(name);
                        label_name = name.to_string();
                    },
                    _ => is_known = false,
                }
                let mut new_args = Vec::new();
                for arg_nd in args.iter() {
                    new_args.push(self.closure(env.clone(), known.clone(), arg_nd));
                }
                if !is_known {
                    CNode::AppCls {
                        func: Box::new(new_func),
                        args: new_args,
                        func_ty: func_ty.clone(),
                        ty: ty.clone(),
                    }
                } else {
                    CNode::AppDir {
                        func: label_name,
                        args: new_args,
                        func_ty: func_ty.clone(),
                        ty: ty.clone(),
                    }
                }
            }
        }
    }
}

pub fn closure(node: TypedNode) -> (Vec<FunDef>, CNode) {
    let mut clos = Closure::new();
    let new_node = clos.closure(Env::new(), Known::new(), &node);
    
    (clos.toplevel.into_iter().rev().collect(), new_node)
}
