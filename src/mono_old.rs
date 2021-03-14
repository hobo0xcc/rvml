use crate::closure::*;
use crate::typing::{Subst, Type, TypeVar};
use rpds::{HashTrieSet, HashTrieMap};
use std::collections::HashMap;

type Env = HashTrieMap<String, Type>;

// Monomorphization
pub struct Mono {
    func_map: HashMap<String, FunDef>,
    generated: HashTrieSet<String>,
    dups: HashMap<String, Vec<(String, Type)>>,
    pub toplevel: Vec<FunDef>,
}

impl Mono {
    pub fn new() -> Mono {
        Mono {
            func_map: HashMap::new(),
            generated: HashTrieSet::new(),
            dups: HashMap::new(),
            toplevel: Vec::new(),
        }
    }

    pub fn get_func(&self, name: &String) -> FunDef {
        self.func_map.get(name).unwrap().clone()
    }

    pub fn is_function(&self, name: &String) -> bool {
        self.func_map.contains_key(name)
    }

    pub fn synthesize_subst(&self, s1: &Subst, s2: &Subst) -> Subst {
        let mut new_subst = Subst::new();
        for (key, val) in s1.iter() {
            new_subst = new_subst.insert(key.clone(), val.clone());
        }
        for (key, val) in s2.iter() {
            new_subst = new_subst.insert(key.clone(), val.clone());
        }

        new_subst
    }

    pub fn duplicate(&mut self, subst: &Subst, name: &String) -> (String, Type) {
        let mut new_env = Env::new();
        let from_func = self.get_func(name);
        let (ref name, ref ty) = from_func.name;
        let new_ty = self.apply_subst_type(subst, ty);
        let new_name = self.mangle_name(name, &new_ty);
        if self.generated.contains(&new_name) {
            return (new_name, new_ty);
        }
        new_env = new_env.insert(name.clone(), ty.clone());
        self.generated = self.generated.insert(new_name.clone());
        let mut new_args = Vec::new();
        for (arg_name, arg_ty) in from_func.args.iter() {
            let ty = self.apply_subst_type(subst, arg_ty);
            new_args.push((arg_name.to_string(), ty.clone()));
            new_env = new_env.insert(arg_name.to_string(), ty);
        }

        let mut new_formal_fv = Vec::new();
        println!("func: {}", name);
        println!("freevar: {:?}", from_func.formal_fv);
        for (name, ty) in from_func.formal_fv.iter() {
            println!("{}", name);
            if self.is_function(name) {
                let dummy_dups = vec![];
                let dups = self.dups.get(name).unwrap_or(&dummy_dups);
                for (name_dup, ty_dup) in dups.iter() {
                    let ty_dup_app = self.apply_subst_type(subst, ty_dup);
                    println!("dup: {}", ty_dup_app);
                    new_formal_fv.push((name_dup.clone(), ty_dup_app.clone()));
                    new_env = new_env.insert(name_dup.clone(), ty_dup_app.clone());
                }
            } else {
                new_formal_fv.push((name.clone(), ty.clone()));
                new_env = new_env.insert(name.clone(), ty.clone());
            }
        }

        let new_body = self.apply_subst_node(new_env, subst, &from_func.body);

        let new_fundef = FunDef {
            name: (new_name.clone(), new_ty.clone()),
            args: new_args,
            formal_fv: new_formal_fv,
            body: new_body,
            is_recurs: from_func.is_recurs,
        };

        self.toplevel.push(new_fundef);

        (new_name, new_ty)
    }

    pub fn apply_subst_node(&mut self, env: Env, subst: &Subst, node: &CNode) -> CNode {
        match *node {
            CNode::VarExpr(ref name, ref ty, ref subst_var, ref is_extern) => {
                let new_ty = self.apply_subst_type(&subst, ty);
                let new_name = if self.func_map.contains_key(name) {
                    let new_subst = self.synthesize_subst(subst, subst_var);
                    let (func_name, ty) = self.duplicate(&new_subst, name);
                    self.dups
                        .entry(name.to_string())
                        .or_insert_with(|| vec![])
                        .push((func_name.clone(), ty));
                    func_name
                } else {
                    name.to_string()
                };

                CNode::VarExpr(new_name, new_ty, subst_var.clone(), *is_extern)
            }
            CNode::Not(ref expr) => CNode::Not(Box::new(self.apply_subst_node(env, subst, &**expr))),
            CNode::Neg(ref expr) => CNode::Neg(Box::new(self.apply_subst_node(env, subst, &**expr))),
            CNode::FNeg(ref expr) => CNode::FNeg(Box::new(self.apply_subst_node(env, subst, &**expr))),
            CNode::Tuple(ref nds, ref ty) => {
                let new_ty = self.apply_subst_type(subst, ty);
                let mut new_nds = Vec::new();
                for nd in nds.iter() {
                    new_nds.push(self.apply_subst_node(env.clone(), subst, nd));
                }

                CNode::Tuple(new_nds, new_ty)
            }
            CNode::Array(ref size, ref expr, ref ty) => {
                let new_size = self.apply_subst_node(env.clone(), subst, size);
                let new_expr = self.apply_subst_node(env.clone(), subst, expr);
                let new_ty = self.apply_subst_type(subst, ty);

                CNode::Array(Box::new(new_size), Box::new(new_expr), new_ty)
            }
            CNode::Get(ref array, ref idx, ref ty) => {
                let new_array = self.apply_subst_node(env.clone(), subst, array);
                let new_idx = self.apply_subst_node(env.clone(), subst, idx);
                let new_ty = self.apply_subst_type(subst, ty);

                CNode::Get(Box::new(new_array), Box::new(new_idx), new_ty)
            }
            CNode::Put(ref array, ref idx, ref expr, ref ty) => {
                let new_array = self.apply_subst_node(env.clone(), subst, array);
                let new_idx = self.apply_subst_node(env.clone(), subst, idx);
                let new_expr = self.apply_subst_node(env.clone(), subst, expr);
                let new_ty = self.apply_subst_type(subst, ty);

                CNode::Put(
                    Box::new(new_array),
                    Box::new(new_idx),
                    Box::new(new_expr),
                    new_ty,
                )
            }
            CNode::Expr {
                ref lhs,
                ref op,
                ref rhs,
                ref ty,
            } => {
                let new_lhs = self.apply_subst_node(env.clone(), subst, &**lhs);
                let new_rhs = self.apply_subst_node(env.clone(), subst, &**rhs);
                let new_ty = self.apply_subst_type(subst, ty);

                CNode::Expr {
                    lhs: Box::new(new_lhs),
                    op: op.to_string(),
                    rhs: Box::new(new_rhs),
                    ty: new_ty,
                }
            }
            CNode::IfExpr {
                ref cond,
                ref then_body,
                ref else_body,
                ref ty,
            } => {
                let new_cond = self.apply_subst_node(env.clone(), subst, &**cond);
                let new_then_body = self.apply_subst_node(env.clone(), subst, &**then_body);
                let new_else_body = self.apply_subst_node(env.clone(), subst, &**else_body);
                let new_ty = self.apply_subst_type(subst, ty);

                CNode::IfExpr {
                    cond: Box::new(new_cond),
                    then_body: Box::new(new_then_body),
                    else_body: Box::new(new_else_body),
                    ty: new_ty,
                }
            }
            CNode::LetExpr {
                ref name,
                ref first_expr,
                ref second_expr,
                ty: ref _ty,
            } => {
                let (ref id, ref id_ty) = name;
                let ty = self.apply_subst_type(subst, id_ty);
                let new_name = (id.to_string(), ty.clone());
                let new_first_expr = self.apply_subst_node(env.clone(), subst, &**first_expr);
                let second_env = env.insert(id.to_string(), ty.clone());
                let new_second_expr = self.apply_subst_node(second_env, subst, &**second_expr);
                let new_ty = self.apply_subst_type(subst, &ty);

                CNode::LetExpr {
                    name: new_name,
                    first_expr: Box::new(new_first_expr),
                    second_expr: Box::new(new_second_expr),
                    ty: new_ty,
                }
            }
            CNode::LetTupleExpr {
                ref names,
                ref first_expr,
                ref second_expr,
                ref tuple_ty,
                ref ty,
            } => {
                let mut new_names = Vec::new();
                let mut second_env = env.clone();
                for (id, id_ty) in names.iter() {
                    let ty = self.apply_subst_type(subst, id_ty);
                    new_names.push((id.to_string(), ty.clone()));
                    second_env = second_env.insert(id.to_string(), ty);
                }
                let new_first_expr = self.apply_subst_node(env.clone(), subst, &**first_expr);
                let new_second_expr = self.apply_subst_node(second_env, subst, &**second_expr);
                let new_tuple_ty = self.apply_subst_type(subst, tuple_ty);
                let new_ty = self.apply_subst_type(subst, ty);

                CNode::LetTupleExpr {
                    names: new_names,
                    first_expr: Box::new(new_first_expr),
                    second_expr: Box::new(new_second_expr),
                    tuple_ty: new_tuple_ty,
                    ty: new_ty,
                }
            }
            CNode::MakeCls {
                ref name,
                dups: ref _dups,
                ref actual_fv,
                ref second_expr,
                ref ty,
            } => {
                let (ref id, ref id_ty) = name;
                let new_env = env.insert(id.to_string(), id_ty.clone());
                let poly_dups_backup = self.dups.get(id).unwrap_or(&vec![]).clone();
                let new_second_expr = self.apply_subst_node(new_env, subst, &**second_expr);
                let poly_dups = self.dups.get(id).unwrap_or(&vec![]).clone();

                let dup_map: HashMap<String, Type> = poly_dups_backup.into_iter().collect();
                let mut new_dups = Vec::new();
                for (name, ty) in poly_dups.into_iter() {
                    if !dup_map.contains_key(&name) {
                        new_dups.push((name, ty));
                    }
                }

                let (new_name_id, new_name_ty, new_ty) = if new_dups.len() == 0 {
                    let new_name_ty = self.apply_subst_type(subst, id_ty);
                    let new_name_id = self.mangle_name(id, &new_name_ty);
                    let new_ty = self.apply_subst_type(subst, ty);
                    (new_name_id, new_name_ty, new_ty)
                } else {
                    (
                        id.to_string(),
                        id_ty.clone(),
                        self.apply_subst_type(subst, ty),
                    )
                };

                let mut new_fv = Vec::new();
                for fv in actual_fv.iter() {
                    println!("actual_fv: {}: {}", fv, env.get(fv).unwrap());
                    let ty = self.apply_subst_type(subst, env.get(fv).unwrap());
                    let name = if self.is_function(fv) {
                        self.mangle_name(fv, &ty)
                    } else {
                        fv.to_string()
                    };
                    new_fv.push(name);
                }

                CNode::MakeCls {
                    name: (new_name_id, new_name_ty),
                    dups: Some(new_dups),
                    actual_fv: new_fv,
                    second_expr: Box::new(new_second_expr),
                    ty: new_ty,
                }
            }
            CNode::AppCls {
                ref func,
                ref args,
                ref func_ty,
                ref ty,
            } => {
                let new_func = self.apply_subst_node(env.clone(), subst, &**func);
                let mut new_args = Vec::new();
                for arg in args.iter() {
                    new_args.push(self.apply_subst_node(env.clone(), subst, arg));
                }
                let new_func_ty = self.apply_subst_type(subst, func_ty);
                let new_ty = self.apply_subst_type(subst, ty);

                CNode::AppCls {
                    func: Box::new(new_func),
                    args: new_args,
                    func_ty: new_func_ty,
                    ty: new_ty,
                }
            }
            CNode::AppDir {
                ref func,
                ref args,
                ref func_ty,
                ref ty,
            } => {
                let new_func = self.apply_subst_node(env.clone(), subst, &**func);
                let mut new_args = Vec::new();
                for arg in args.iter() {
                    new_args.push(self.apply_subst_node(env.clone(), subst, arg));
                }
                let new_func_ty = self.apply_subst_type(subst, func_ty);
                let new_ty = self.apply_subst_type(subst, ty);

                CNode::AppDir {
                    func: Box::new(new_func),
                    args: new_args,
                    func_ty: new_func_ty,
                    ty: new_ty,
                }
            }
            _ => node.clone(),
        }
    }

    pub fn apply_subst_type(&self, subst: &Subst, ty: &Type) -> Type {
        match *ty {
            Type::Array(ref ty) => Type::Array(Box::new(self.apply_subst_type(subst, &**ty))),
            Type::Tuple(ref types) => {
                let mut new_types = Vec::new();
                for ty in types.iter() {
                    new_types.push(self.apply_subst_type(subst, ty));
                }

                Type::Tuple(new_types)
            }
            Type::Func { ref args, ref ret } => {
                let mut new_args = Vec::new();
                for arg in args.iter() {
                    new_args.push(self.apply_subst_type(subst, arg));
                }
                let new_ret = self.apply_subst_type(subst, &*ret);

                Type::Func {
                    args: new_args,
                    ret: Box::new(new_ret),
                }
            }
            Type::QVar(ref id) => {
                println!("subst: {}", subst);
                println!("{}", id);
                // Type::Unit
                self.apply_subst_type(subst, subst.get(id).unwrap())
            }
            Type::TVar(ref tv) => match tv.get() {
                TypeVar::Unbound(id, _) => {
                    let ty_opt = subst.get(&id);
                    match ty_opt {
                        Some(ty) => self.apply_subst_type(subst, ty),
                        None => Type::Unit,
                    }
                }
                _ => unreachable!(),
            },
            _ => ty.clone(),
        }
    }

    pub fn mangle_name(&self, orig_name: &String, ty: &Type) -> String {
        let res = format!("{}_{}", orig_name, ty.to_string());
        res
    }

    pub fn monomorphize(mut self, prog: (Vec<FunDef>, CNode)) -> (Vec<FunDef>, CNode) {
        let (toplevel, node) = prog;

        for fun in toplevel.into_iter() {
            self.func_map.insert(fun.name.0.clone(), fun);
        }

        let subst = Subst::new();
        let new_node = self.apply_subst_node(Env::new(), &subst, &node);

        (self.toplevel, new_node)
    }
}

pub fn mono(prog: (Vec<FunDef>, CNode)) -> (Vec<FunDef>, CNode) {
    println!("Mono");
    let m = Mono::new();
    m.monomorphize(prog)
}
