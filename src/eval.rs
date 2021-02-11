use super::parse::*;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Int(i32),
    Func {
        body: Box<Node>,
        env: Rc<RefCell<Environment<Object>>>,
        args: Vec<String>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Environment<T>
where T: Clone {
    curr: HashMap<String, T>,
    parent: Option<Rc<RefCell<Environment<T>>>>,
}

impl<T> Environment<T>
where T: Clone {
    pub fn new() -> Environment<T> {
        Environment {
            curr: HashMap::new(),
            parent: None,
        }
    }

    pub fn make_child(parent: Rc<RefCell<Environment<T>>>) -> Environment<T> {
        Environment {
            curr: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn get(&self, key: &str) -> Option<T> {
        if let Some(item) = self.curr.get(key) {
            return Some(item.clone());
        } else if let Some(parent) = self.parent.clone() {
            return match parent.borrow().get(key) {
                Some(item) => Some(item.clone()),
                None => None,
            };
        } else {
            return None;
        }
    }

    pub fn set(&mut self, key: String, item: T) {
        self.curr.insert(key, item);
    }
}

#[derive(Debug)]
pub enum EvalError {
    UndefinedVarAccess {
        name: String,
    },
}

pub struct Eval {
    env: Rc<RefCell<Environment<Object>>>,
}

impl Eval {
    pub fn new() -> Eval {
        let curr = Environment::new();
        Eval {
            env: Rc::new(RefCell::new(curr)),
        }
    }

    pub fn from(env: Rc<RefCell<Environment<Object>>>) -> Eval {
        Eval {
            env,
        }
    }

    pub fn get(&mut self, key: String) -> Option<Object> {
        self.env.borrow().get(&key)
    }

    pub fn bind(&mut self, key: String, item: Object) {
        self.env.borrow_mut().set(key, item);
    }

    pub fn eval(&mut self, node: &Node) -> Result<Object, EvalError> {
        match node {
            Node::Int(n) => {
                return Ok(Object::Int(*n));
            }
            Node::VarExpr(name) => {
                let result = self.get(name.to_string());
                return match result {
                    Some(obj) => Ok(obj),
                    None => Err(EvalError::UndefinedVarAccess { name: name.to_string() }),
                };
            }
            Node::Expr { lhs, op, rhs } => {
                let lval = match self.eval(&*lhs)? {
                    Object::Int(n) => n,
                    _ => unreachable!(),
                };
                let rval = match self.eval(&*rhs)? {
                    Object::Int(n) => n,
                    _ => unreachable!(),
                };
                Ok(Object::Int(match op.as_str() {
                    "+" => lval + rval,
                    "-" => lval - rval,
                    "*" => lval * rval,
                    "/" => lval * rval,
                    _ => unreachable!(),
                }))
            }
            Node::IfExpr {
                cond,
                then_body,
                else_body,
            } => {
                let cond_int = match self.eval(&*cond)? {
                    Object::Int(n) => n,
                    _ => unreachable!(),
                };
                if cond_int != 0 {
                    return self.eval(&*then_body);
                } else {
                    return self.eval(&*else_body);
                }
            }
            Node::LetExpr {
                name,
                first_expr,
                second_expr,
            } => {
                let result = self.eval(&*first_expr)?;
                self.bind(name.to_string(), result);
                return Ok(self.eval(&*second_expr)?);
            }
            Node::LetRecExpr {
                name,
                args,
                first_expr,
                second_expr,
            } => {
                let new_env = Environment::make_child((&self.env).clone());
                let func_env = Rc::new(RefCell::new(new_env));
                let func_obj = Object::Func {
                    body: first_expr.clone(),
                    env: func_env,
                    args: args.clone(),
                };
                self.bind(name.to_string(), func_obj);
                let result = self.eval(&*second_expr)?;
                Ok(result)
            }
            Node::App { func, args } => {
                let (func_body, func_env, func_args) = match self.eval(&*func)? {
                    Object::Func { body, env, args } => (body, env, args),
                    _ => unreachable!(),
                };
                let mut new_func_env = Environment::make_child(func_env);
                let mut cnt: usize = 0;
                for (name, node) in func_args.iter().zip(args.into_iter()) {
                    new_func_env.set(name.to_string(), self.eval(&node)?);
                    cnt += 1;
                }
                let env_rc = Rc::new(RefCell::new(new_func_env));
                if cnt >= func_args.len() {
                    let mut func_eval = Eval::from(env_rc);
                    let result = func_eval.eval(&*func_body)?;
                    return Ok(result);
                } else {
                    return Ok(Object::Func {
                        body: func_body,
                        env: env_rc,
                        args: func_args[cnt..].to_vec(),
                    });
                }
            }
        }
    }
}

pub fn eval(node: Node) -> Result<Object, EvalError> {
    let mut e = Eval::new();
    e.eval(&node)
}
