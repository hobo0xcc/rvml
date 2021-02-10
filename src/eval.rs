use super::parse::*;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Object {
    Int(i32),
    Func(Box<Node>),
}

pub struct EvalEnv {
    bindings: Scope<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope<T> {
    curr: HashMap<String, T>,
    parent: Option<Rc<Scope<T>>>,
}

impl<T> Scope<T> {
    pub fn new() -> Scope<T> {
        Scope {
            curr: HashMap::new(),
            parent: None,
        }
    }

    pub fn set_parent(&mut self, parent: Scope<T>) {
        self.parent = Some(Rc::new(parent));
    }

    pub fn get(&self, key: &String) -> Option<&T> {
        if let Some(item) = self.curr.get(key) {
            return Some(item);
        } else if let Some(parent) = &self.parent {
            let data = (*parent).get(key);
            return data;
        } else {
            return None;
        }
    }

    pub fn put(&mut self, key: String, item: T) {
        self.curr.insert(key, item);
    }
}

impl EvalEnv {
    pub fn new(scope: Option<Scope<Node>>) -> EvalEnv {
        EvalEnv {
            bindings: if let Some(s) = scope { s } else { Scope::new() },
        }
    }
}

pub fn eval<'a>(node: Node, env: &mut EvalEnv) -> Object {
    match node {
        Node::Int(n) => {
            return Object::Int(n);
        },
        Node::VarExpr(name) => {
            return eval(env.bindings.get(&name).unwrap().clone(), env);
        },
        Node::Expr { lhs, op, rhs } => {
            let lval = match eval(*lhs, env) { Object::Int(n) => n, _ => unreachable!() };
            let rval = match eval(*rhs, env) { Object::Int(n) => n, _ => unreachable!() };
            Object::Int(match op.as_str() {
                "+" => lval + rval,
                "-" => lval - rval,
                "*" => lval * rval,
                "/" => lval * rval,
                _ => unreachable!()
            })
        },
        Node::IfExpr { cond, then_body, else_body } => {
            let cond_int = match eval(*cond, env) { Object::Int(n) => n, _ => unreachable!() };
            if cond_int != 0 {
                return eval(*then_body, env);
            } else {
                return eval(*else_body, env);
            }
        },
        Node::LetExpr { name, first_expr, second_expr } => {
            env.bindings.put(name, *first_expr);
            return eval(*second_expr, env);
        },
        Node::LetRecExpr { name, args, first_expr, second_expr } => {
            let mut new_bindings = Scope::new();
            new_bindings.set_parent(env.bindings.clone());
            let curry_func = Node::CurryFunc {
                body: first_expr,
                env: new_bindings,
                args,
            };
            env.bindings.put(name, curry_func);
            eval(*second_expr, env)
        },
        Node::CurryFunc { body, env: func_scope, args } => {
            if args.len() == 0 {
                let mut new_env = EvalEnv::new(Some(func_scope));
                new_env.bindings.set_parent(env.bindings.clone());
                return eval(*body, &mut new_env);
            } else {
                let new_node = Node::CurryFunc {
                    body,
                    env: func_scope,
                    args,
                };
                return Object::Func(Box::new(new_node));
            }
        },
        Node::App { func, args } => {
            let mut curry_func = match eval(*func, env) { Object::Func(node) => node, _ => unreachable!() };
            let (func_body, func_env, func_args) = match *curry_func { Node::CurryFunc { body, env, args } => (body, env, args), _ => unreachable!() };
            let mut new_func_env = func_env.clone();
            let mut cnt: usize = 0;
            for (name, node) in func_args.iter().zip(args.into_iter()) {
                new_func_env.put(name.to_string(), node);
                cnt += 1;
            }
            let new_curry_func = Node::CurryFunc {
                body: func_body,
                env: new_func_env,
                args: func_args[cnt..].to_vec()
            };
            eval(new_curry_func, env)
        }
    }
}