use super::parse::*;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Int(i32),
    Func(Box<Node>),
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
    UndefinedVarAccess,
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
                    None => Err(EvalError::UndefinedVarAccess),
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
                let curry_func = Node::CurryFunc {
                    body: first_expr.clone(),
                    env: func_env,
                    args: args.clone(),
                };
                let result = self.eval(&curry_func)?;
                self.bind(name.to_string(), result);
                let result = self.eval(&*second_expr)?;
                Ok(result)
            }
            Node::CurryFunc {
                body,
                env: func_scope,
                args,
            } => {
                if args.len() == 0 {
                    let mut func_eval = Eval::from(func_scope.clone());
                    let result = func_eval.eval(&*body)?;
                    return Ok(result);
                } else {
                    let new_node = Node::CurryFunc {
                        body: body.clone(),
                        env: func_scope.clone(),
                        args: args.clone(),
                    };
                    return Ok(Object::Func(Box::new(new_node)));
                }
            }
            Node::App { func, args } => {
                let curry_func = match self.eval(&*func)? {
                    Object::Func(node) => node,
                    _ => unreachable!(),
                };
                let (func_body, func_env, func_args) = match *curry_func {
                    Node::CurryFunc { body, env, args } => (body, env, args),
                    _ => unreachable!(),
                };
                let new_func_env = func_env;
                let mut cnt: usize = 0;
                for (name, node) in func_args.iter().zip(args.into_iter()) {
                    new_func_env.borrow_mut().set(name.to_string(), self.eval(&node)?);
                    cnt += 1;
                }
                let new_curry_func = Node::CurryFunc {
                    body: func_body,
                    env: new_func_env,
                    args: func_args[cnt..].to_vec(),
                };
                self.eval(&new_curry_func)
            }
        }
    }
}

pub fn eval(node: Node) -> Result<Object, EvalError> {
    let mut e = Eval::new();
    e.eval(&node)
}