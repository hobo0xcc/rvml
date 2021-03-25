use crate::typing::{is_primitive, TypedNode};
use rpds::HashTrieMap;

pub struct Alpha {
    counter: usize,
}

type Env = HashTrieMap<String, String>;

impl Alpha {
    pub fn new() -> Alpha {
        Alpha { counter: 0 }
    }

    pub fn gen_id(&mut self, s: &String) -> String {
        let new_s = format!("{}.{}", s, self.counter);
        self.counter += 1;
        new_s
    }

    pub fn find(&self, env: Env, key: &String) -> String {
        match env.get(key) {
            Some(item) => item.clone(),
            None => key.clone(),
        }
    }

    pub fn alpha(&mut self, env: Env, node: &mut TypedNode) {
        use crate::typing::TypedNode::*;
        match *node {
            Unit => {}
            Int(ref _n) => {}
            Float(ref _f) => {}
            Bool(ref _b) => {}
            VarExpr(ref mut name, ref _ty, _) => {
                if !is_primitive(name) {
                    *name = self.find(env, name);
                }
            }
            VarExtExpr(ref mut _name, ref _ty) => {}
            Not(ref mut expr) => self.alpha(env, expr),
            Neg(ref mut expr) => self.alpha(env, expr),
            FNeg(ref mut expr) => self.alpha(env, expr),
            Tuple(ref mut nds, ref _ty) => {
                for nd in nds.iter_mut() {
                    self.alpha(env.clone(), nd);
                }
            }
            Array(ref mut size, ref mut expr, ref _ty) => {
                self.alpha(env.clone(), &mut **size);
                self.alpha(env.clone(), &mut **expr);
            }
            Get(ref mut array, ref mut idx, ref _ty) => {
                self.alpha(env.clone(), &mut **array);
                self.alpha(env.clone(), &mut **idx);
            }
            Put(ref mut array, ref mut idx, ref mut new_expr, ref _ty) => {
                self.alpha(env.clone(), &mut **array);
                self.alpha(env.clone(), &mut **idx);
                self.alpha(env.clone(), &mut **new_expr);
            }
            Expr {
                ref mut lhs,
                op: ref _op,
                ref mut rhs,
                ty: ref _ty,
            } => {
                self.alpha(env.clone(), &mut **lhs);
                self.alpha(env.clone(), &mut **rhs);
            }
            IfExpr {
                ref mut cond,
                ref mut then_body,
                ref mut else_body,
                ty: ref _ty,
            } => {
                self.alpha(env.clone(), &mut **cond);
                self.alpha(env.clone(), &mut **then_body);
                self.alpha(env.clone(), &mut **else_body);
            }
            LetExpr {
                ref mut name,
                ref mut first_expr,
                ref mut second_expr,
                ty: ref _ty,
            } => {
                let (ref mut id, ref _id_ty) = name;
                self.alpha(env.clone(), &mut **first_expr);
                let new_id = self.gen_id(id);
                self.alpha(env.insert(id.clone(), new_id.clone()), &mut **second_expr);
                *id = new_id;
            }
            LetTupleExpr {
                ref mut names,
                ref mut first_expr,
                ref mut second_expr,
                tuple_ty: ref _tuple_ty,
                ty: ref _ty,
            } => {
                self.alpha(env.clone(), &mut **first_expr);
                let mut new_env = env.clone();
                for (id, _id_ty) in names.iter_mut() {
                    let new_id = self.gen_id(id);
                    new_env = new_env.insert(id.clone(), new_id.clone());
                    *id = new_id;
                }
                self.alpha(new_env, &mut **second_expr);
            }
            LetRecExpr {
                ref mut name,
                ref mut args,
                ref mut first_expr,
                ref mut second_expr,
                ty: ref _ty,
            } => {
                let (ref mut id, ref mut _id_ty) = name;
                let mut env_e2 = Env::new();
                if !is_primitive(id) {
                    let new_id = self.gen_id(id);
                    env_e2 = env.insert(id.clone(), new_id.clone());
                    *id = new_id;
                }

                let mut env_e1 = env_e2.clone();

                for arg in args.iter_mut() {
                    let (ref mut id, ref mut _id_ty) = arg;
                    let new_id = self.gen_id(id);
                    env_e1 = env_e1.insert(id.clone(), new_id.clone());
                    *id = new_id;
                }

                self.alpha(env_e1, &mut **first_expr);
                self.alpha(env_e2, &mut **second_expr);
            }
            App {
                ref mut func,
                ref mut args,
                func_ty: ref _func_ty,
                ty: ref _ty,
            } => {
                self.alpha(env.clone(), &mut **func);
                for arg in args.iter_mut() {
                    self.alpha(env.clone(), arg);
                }
            }
        }
    }
}

pub fn alpha(mut node: TypedNode) -> TypedNode {
    // println!("Alpha");
    let mut a = Alpha::new();
    a.alpha(Env::new(), &mut node);
    node
}
