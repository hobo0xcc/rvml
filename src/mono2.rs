use crate::typing::*;
use std::collections::HashMap;
use rpds::{HashTrieMap, HashTrieSet};

type REnv = HashMap<String, Vec<Subst>>;
type Env = HashTrieMap<String, TypedNode>;

pub struct Mono2 {
    renv: REnv,
    visited: HashMap<String, bool>,
    resolved: HashTrieSet<String>,
}

impl Mono2 {
    pub fn new() -> Mono2 {
        Mono2 {
            renv: REnv::new(),
            visited: HashMap::new(),
            resolved: HashTrieSet::new(),
        }
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

    pub fn is_poly(&self, ty: &Type) -> bool {
        match *ty {
            Type::Array(ref ty1) => {
                self.is_poly(&**ty1)
            }
            Type::Tuple(ref tys) => {
                let mut flag = false;
                for ty in tys.iter() {
                    flag |= self.is_poly(ty);
                }

                flag
            }
            Type::Func { ref args, ref ret } => {
                let mut flag = false;
                for ty in args.iter() {
                    flag |= self.is_poly(ty);
                }
                flag |= self.is_poly(&**ret);
                flag
            }
            Type::TVar(ref tv) => {
                match tv.get() {
                    TypeVar::Unbound(_, _) => true,
                    _ => false,
                }
            }
            Type::QVar(_) => true,
            _ => false,
        }
    }

    pub fn apply_subst(&self, subst: &Subst, ty: &Type) -> Type {
        match *ty {
            Type::Array(ref ty) => Type::Array(Box::new(self.apply_subst(subst, &**ty))),
            Type::Tuple(ref types) => {
                let mut new_types = Vec::new();
                for ty in types.iter() {
                    new_types.push(self.apply_subst(subst, ty));
                }

                Type::Tuple(new_types)
            }
            Type::Func { ref args, ref ret } => {
                let mut new_args = Vec::new();
                for arg in args.iter() {
                    new_args.push(self.apply_subst(subst, arg));
                }
                let new_ret = self.apply_subst(subst, &*ret);

                Type::Func {
                    args: new_args,
                    ret: Box::new(new_ret),
                }
            }
            Type::QVar(ref id) => {
                let id_ty = subst.get(id);
                match id_ty {
                    Some(ty) => self.apply_subst(subst, ty),
                    None => ty.clone(),
                }
            }
            Type::TVar(ref tv) => match tv.get() {
                TypeVar::Unbound(id, _) => {
                    let ty_opt = subst.get(&id);
                    match ty_opt {
                        Some(ty) => self.apply_subst(subst, ty),
                        None => ty.clone(),
                    }
                }
                _ => unreachable!(),
            },
            _ => ty.clone(),
        }
    }

    pub fn resolve(&mut self, env: Env, subst: &Subst, node: &TypedNode) {
        use crate::typing::TypedNode::*;
        match *node {
            Unit | Int(_) | Float(_) | Bool(_) => {},
            VarExpr(ref name, ref ty, ref subst_var) => {
                // println!("{}", name);
                let new_subst = self.synthesize_subst(subst, subst_var);
                let new_ty = self.apply_subst(&new_subst, ty);
                if !self.is_poly(&new_ty) && env.contains_key(name) {
                    let mangled_name = self.mangle_name(name, &new_ty);
                    if self.resolved.contains(&mangled_name) {
                        return;
                    }
                    self.resolved = self.resolved.insert(mangled_name);
                    if let Some(flag) = self.visited.get(name) {
                        if *flag {
                            return;
                        }
                    }
                    self.visited.insert(name.to_string(), true);
                    let nd = env.get(name).unwrap();
                    self.resolve(env.clone(), &new_subst, nd);
                    self.visited.insert(name.to_string(), false);
                    self.renv.entry(name.to_string()).or_insert_with(|| vec![]).push(new_subst);
                }
            }
            VarExtExpr(_, _) => {},
            Not(ref expr) => self.resolve(env.clone(), subst, &**expr),
            Neg(ref expr) => self.resolve(env.clone(), subst, &**expr),
            FNeg(ref expr) => self.resolve(env.clone(), subst, &**expr),
            Tuple(ref nds, ref _ty) => {
                for nd in nds.iter() {
                    self.resolve(env.clone(), subst, nd);
                }
            }
            Array(ref size, ref expr, ref _ty) => {
                self.resolve(env.clone(), subst, size);
                self.resolve(env.clone(), subst, expr);
            }
            Get(ref array, ref idx, ref _ty) => {
                self.resolve(env.clone(), subst, array);
                self.resolve(env.clone(), subst, idx);
            }
            Put(ref array, ref idx, ref expr, ref _ty) => {
                self.resolve(env.clone(), subst, array);
                self.resolve(env.clone(), subst, idx);
                self.resolve(env.clone(), subst, expr);
            }
            Expr {
                ref lhs,
                op: ref _op,
                ref rhs,
                ty: ref _ty,
            } => {
                self.resolve(env.clone(), subst, &**lhs);
                self.resolve(env.clone(), subst, &**rhs);
            }
            IfExpr {
                ref cond,
                ref then_body,
                ref else_body,
                ty: ref _ty,
            } => {
                self.resolve(env.clone(), subst, &**cond);
                self.resolve(env.clone(), subst, &**then_body);
                self.resolve(env.clone(), subst, &**else_body);
            }
            LetExpr {
                ref name,
                ref first_expr,
                ref second_expr,
                ty: ref _ty,
            } => {
                let (ref id, ref _id_ty) = name;
                let new_env = env.insert(id.to_string(), (**first_expr).clone());
                self.resolve(env.clone(), subst, &**first_expr);
                self.resolve(new_env.clone(), subst, &**second_expr);
            }
            LetTupleExpr {
                names: ref _names,
                ref first_expr,
                ref second_expr,
                tuple_ty: ref _tuple_ty,
                ty: ref _ty,
            } => {
                self.resolve(env.clone(), subst, &**first_expr);
                self.resolve(env.clone(), subst, &**second_expr);
            }
            LetRecExpr {
                ref name,
                args: ref _args,
                ref first_expr,
                ref second_expr,
                ty: ref _ty,
            } => {
                let (ref id, ref _id_ty) = name;
                let new_env = env.insert(id.to_string(), (**first_expr).clone());
                self.resolve(new_env.clone(), subst, &**first_expr);
                self.resolve(new_env.clone(), subst, &**second_expr);
            }
            App {
                ref func,
                ref args,
                func_ty: ref _func_ty,
                ty: ref _ty,
            } => {
                self.resolve(env.clone(), subst, &**func);
                for arg in args.iter() {
                    self.resolve(env.clone(), subst, arg);
                }
            }
        }
    }

    pub fn mangle_name(&self, name: &String, ty: &Type) -> String {
        format!("{}_{}", name, ty.to_string())
    }

    pub fn duplicate(&self, subst: &Subst, node: &TypedNode) -> TypedNode {
        use crate::typing::TypedNode::*;
        match *node {
            Unit | Int(_) | Float(_) | Bool(_) => node.clone(),
            VarExpr(ref name, ref ty, ref subst_var) => {
                let new_ty = self.apply_subst(subst, ty);
                let new_name = if is_primitive(name) {
                    name.to_string()
                } else {
                    // println!("{}: {}", name, new_ty);
                    self.mangle_name(name, &new_ty)
                };

                VarExpr(new_name, new_ty, subst_var.clone())
            }
            VarExtExpr(_, _) => node.clone(),
            Not(ref expr) => Not(Box::new(self.duplicate(subst, &**expr))),
            Neg(ref expr) => Neg(Box::new(self.duplicate(subst, &**expr))),
            FNeg(ref expr) => FNeg(Box::new(self.duplicate(subst, &**expr))),
            Tuple(ref nds, ref ty) => {
                let new_ty = self.apply_subst(subst, ty);
                let mut new_nds = Vec::new();
                for nd in nds.iter() {
                    new_nds.push(self.duplicate(subst, nd));
                }

                Tuple(new_nds, new_ty)
            }
            Array(ref size, ref expr, ref ty) => {
                let new_ty = self.apply_subst(subst, ty);
                let new_size = self.duplicate(subst, &**size);
                let new_expr = self.duplicate(subst, &**expr);

                Array(Box::new(new_size), Box::new(new_expr), new_ty)
            }
            Get(ref array, ref idx, ref ty) => {
                let new_ty = self.apply_subst(subst, ty);
                let new_array = self.duplicate(subst, &**array);
                let new_idx = self.duplicate(subst, &**idx);

                Get(Box::new(new_array), Box::new(new_idx), new_ty)
            }
            Put(ref array, ref idx, ref expr, ref ty) => {
                let new_ty = self.apply_subst(subst, ty);
                let new_array = self.duplicate(subst, &**array);
                let new_idx = self.duplicate(subst, &**idx);
                let new_expr = self.duplicate(subst, &**expr);

                Put(Box::new(new_array), Box::new(new_idx), Box::new(new_expr), new_ty)
            }
            Expr {
                ref lhs,
                ref op,
                ref rhs,
                ref ty,
            } => {
                let new_ty = self.apply_subst(subst, ty);
                let new_lhs = self.duplicate(subst, &**lhs);
                let new_op = op.clone();
                let new_rhs = self.duplicate(subst, &**rhs);

                Expr {
                    lhs: Box::new(new_lhs),
                    op: new_op,
                    rhs: Box::new(new_rhs),
                    ty: new_ty,
                }
            }
            IfExpr {
                ref cond,
                ref then_body,
                ref else_body,
                ref ty,
            } => {
                let new_ty = self.apply_subst(subst, ty);
                let new_cond = self.duplicate(subst, &**cond);
                let new_then_body = self.duplicate(subst, &**then_body);
                let new_else_body = self.duplicate(subst, &**else_body);

                IfExpr {
                    cond: Box::new(new_cond),
                    then_body: Box::new(new_then_body),
                    else_body: Box::new(new_else_body),
                    ty: new_ty,
                }
            }
            LetExpr {
                ref name,
                ref first_expr,
                ref second_expr,
                ref ty,
            } => {
                let new_ty = self.apply_subst(subst, ty);
                let (ref id, ref id_ty) = name;
                let new_name_ty = self.apply_subst(subst, id_ty);
                let new_name = (self.mangle_name(id, &new_name_ty), new_name_ty);
                let new_first_expr = self.duplicate(subst, &**first_expr);
                let new_second_expr = self.duplicate(subst, &**second_expr);

                LetExpr {
                    name: new_name,
                    first_expr: Box::new(new_first_expr),
                    second_expr: Box::new(new_second_expr),
                    ty: new_ty,
                }
            }
            LetTupleExpr {
                ref names,
                ref first_expr,
                ref second_expr,
                ref tuple_ty,
                ref ty,
            } => {
                let new_ty = self.apply_subst(subst, ty);
                let mut new_names = Vec::new();
                for (name, ty) in names.iter() {
                    let new_ty = self.apply_subst(subst, ty);
                    let new_name = self.mangle_name(name, &new_ty);
                    new_names.push((new_name, new_ty));
                }
                let new_first_expr = self.duplicate(subst, &**first_expr);
                let new_second_expr = self.duplicate(subst, &**second_expr);
                let new_tuple_ty = self.apply_subst(subst, tuple_ty);

                LetTupleExpr {
                    names: new_names,
                    first_expr: Box::new(new_first_expr),
                    second_expr: Box::new(new_second_expr),
                    tuple_ty: new_tuple_ty,
                    ty: new_ty,
                }
            }
            LetRecExpr {
                ref name,
                ref args,
                ref first_expr,
                ref second_expr,
                ref ty,
            } => {
                let (ref id, ref id_ty) = name;
                let mut new_second_expr = self.duplicate(subst, &**second_expr);
                let mut generated = HashTrieSet::new();
                for subst in self.renv.get(id).unwrap_or(&vec![]).iter() {
                    let new_name_ty = self.apply_subst(subst, id_ty);
                    let new_name = (self.mangle_name(id, &new_name_ty), new_name_ty);
                    if generated.contains(&new_name.0) {
                        continue;
                    }
                    generated = generated.insert(new_name.0.clone());
                    let new_ty = self.apply_subst(subst, ty);
                    let mut new_args = Vec::new();
                    for (arg_name, arg_ty) in args.iter() {
                        let new_arg_ty = self.apply_subst(subst, arg_ty);
                        let new_arg_name = self.mangle_name(arg_name, &new_arg_ty);
                        new_args.push((new_arg_name, new_arg_ty));
                    }
                    let new_first_expr = self.duplicate(subst, &**first_expr);
                    new_second_expr = LetRecExpr {
                        name: new_name,
                        args: new_args,
                        first_expr: Box::new(new_first_expr),
                        second_expr: Box::new(new_second_expr),
                        ty: new_ty,
                    };
                }

                new_second_expr
            }
            App {
                ref func,
                ref args,
                ref func_ty,
                ref ty,
            } => {
                let new_func = self.duplicate(subst, func);
                let mut new_args = Vec::new();
                for arg in args.iter() {
                    new_args.push(self.duplicate(subst, arg));
                }
                let new_func_ty = self.apply_subst(subst, func_ty);
                let new_ty = self.apply_subst(subst, ty);

                App {
                    func: Box::new(new_func),
                    args: new_args,
                    func_ty: new_func_ty,
                    ty: new_ty,
                }
            }
        }
    }

    pub fn monomorphize(&mut self, node: &TypedNode) -> TypedNode {
        let subst = Subst::new();
        self.resolve(Env::new(), &subst, node);
        self.duplicate(&subst, node)
    }
}

pub fn mono(node: TypedNode) -> TypedNode {
    println!("Mono");
    let mut mono = Mono2::new();
    let res = mono.monomorphize(&node);
    res
}
