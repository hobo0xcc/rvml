use crate::parse::*;
use crate::env::Environment;
use crate::typing::*;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Int(i32),
    Bool(bool),
    Func {
        body: Box<TypedNode>,
        env: Rc<RefCell<Environment<Object>>>,
        args: Vec<(String, Type)>,
    },
}

#[derive(Debug)]
pub enum EvalError {
    UndefinedVarAccess {
        name: String,
    },
    TypeUnmatced {
        expected_type: String,
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

    pub fn eval(&mut self, node: &TypedNode) -> Result<Object, EvalError> {
        match *node {
            TypedNode::Int(ref n) => {
                return Ok(Object::Int(*n));
            }
            TypedNode::Bool(ref b) => {
                return Ok(Object::Bool(*b));
            }
            TypedNode::VarExpr(ref name, ref ty) => {
                let result = self.get(name.to_string());
                return match result {
                    Some(obj) => Ok(obj),
                    None => Err(EvalError::UndefinedVarAccess { name: name.to_string() }),
                };
            }
            TypedNode::Not(ref expr) => {
                let b = self.eval(&*expr)?;
                return match b {
                    Object::Bool(b) => Ok(Object::Bool(!b)),
                    _ => unreachable!(),
                };
            }
            TypedNode::Expr { ref lhs, ref op, ref rhs, ref ty } => {
                match *ty {
                    Type::Int => {
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
                    },
                    Type::Bool => {
                        let lval = match self.eval(&*lhs)? {
                            Object::Int(n) => n,
                            _ => unreachable!(),
                        };
                        let rval = match self.eval(&*rhs)? {
                            Object::Int(n) => n,
                            _ => unreachable!(),
                        };
                        Ok(Object::Bool(match op.as_str() {
                            "<=" => lval <= rval,
                            "=" => lval == rval,
                            _ => unreachable!(),
                        }))
                    },
                    _ => unreachable!(),
                }
            }
            TypedNode::IfExpr {
                ref cond,
                ref then_body,
                ref else_body,
                ref ty,
            } => {
                let cond_bool = match self.eval(&*cond)? {
                    Object::Bool(b) => b,
                    _ => {
                        return Err(EvalError::TypeUnmatced { expected_type: "bool".to_string() })
                    },
                };
                if cond_bool {
                    return self.eval(&*then_body);
                } else {
                    return self.eval(&*else_body);
                }
            }
            TypedNode::LetExpr {
                ref name,
                ref first_expr,
                ref second_expr,
                ref ty,
            } => {
                let result = self.eval(&*first_expr)?;
                let new_env = Environment::make_child((&self.env).clone());
                let env_rc = Rc::new(RefCell::new(new_env));
                let mut new_eval = Eval::from(env_rc.clone());
                new_eval.bind(name.to_string(), result);
                return Ok(new_eval.eval(&*second_expr)?);
            }
            TypedNode::LetRecExpr {
                ref name,
                ref args,
                ref first_expr,
                ref second_expr,
                ref ty,
            } => {
                let new_env = Environment::make_child((&self.env).clone());
                let func_env = Rc::new(RefCell::new(new_env));
                let mut new_eval = Eval::from(func_env.clone());
                let func_obj = Object::Func {
                    body: first_expr.clone(),
                    env: func_env.clone(),
                    args: args.clone(),
                };
                new_eval.bind(name.to_string(), func_obj);
                let result = new_eval.eval(&*second_expr)?;
                Ok(result)
            }
            TypedNode::App { ref func, ref args, ref ty } => {
                let (func_body, func_env, func_args) = match self.eval(&*func)? {
                    Object::Func { body, env, args } => (body, env, args),
                    _ => unreachable!(),
                };
                let mut new_func_env = Environment::make_child(func_env);
                let mut cnt: usize = 0;
                for ((name, ty), node) in func_args.iter().zip(args.into_iter()) {
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

pub fn eval(node: TypedNode) -> Result<Object, EvalError> {
    let mut e = Eval::new();
    e.eval(&node)
}
