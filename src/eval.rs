use crate::env::Environment;
use crate::typing::*;
use crate::closure::*;
use std::{fmt::Formatter, rc::Rc};
use std::cell::RefCell;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Int(i32),
    Bool(bool),
    Tuple(Vec<Object>),
    Closure(Box<Object>, Vec<(String, Object)>),
    Func {
        body: Box<CNode>,
        args: Vec<(String, Type)>,
    },
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            Object::Int(ref n) => write!(f, "{}", n),
            Object::Bool(ref b) => write!(f, "{}", b),
            Object::Tuple(ref objs) => {
                write!(f, "(")?;
                for (i, obj) in objs.iter().enumerate() {
                    if i < objs.len() - 1 {
                        write!(f, "{}, ", obj)?;
                    } else {
                        write!(f, "{}", obj)?;
                    }
                }
                write!(f, ")")
            },
            Object::Closure(ref name, ref fvs) => {
                write!(f, "closure({}, ", name)?;
                for (i, fv) in fvs.iter().enumerate() {
                    if i < fvs.len() {
                        write!(f, "{}, ", fv.0)?;
                    } else {
                        write!(f, "{}", fv.0)?;
                    }
                }
                write!(f, ")")
            },
            Object::Func {
                body: ref _body, 
                ref args,
            } => {
                write!(f, "fun: ")?;
                for (i, (name, _ty)) in args.iter().enumerate() {
                    if i < args.len() - 1 {
                        write!(f, "{} ", name)?;
                    } else {
                        write!(f, "{}", name)?;
                    }
                }
                write!(f, ";")
            }
        }
    }
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
    env: Rc<RefCell<Environment<String, Object>>>,
}

impl Eval {
    pub fn new() -> Eval {
        let curr = Environment::new();
        Eval {
            env: Rc::new(RefCell::new(curr)),
        }
    }

    pub fn from(env: Rc<RefCell<Environment<String, Object>>>) -> Eval {
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

    pub fn eval_toplevel(&mut self, toplevel: Vec<FunDef>) {
        for fndef in toplevel.into_iter() {
            let mut args = Vec::new();
            for arg in fndef.args.into_iter() {
                args.push(arg);
            }
            let func_obj = Object::Func {
                body: Box::new(fndef.body),
                args,
            };
            let (ref id, ref _id_ty) = fndef.name;
            self.bind(id.to_string(), func_obj);
        }
    }

    pub fn eval(&mut self, node: &CNode) -> Result<Object, EvalError> {
        use crate::closure::CNode::*;
        match *node {
            Int(ref n) => {
                return Ok(Object::Int(*n));
            }
            Bool(ref b) => {
                return Ok(Object::Bool(*b));
            }
            VarExpr(ref name, ref _ty) => {
                let result = self.get(name.to_string());
                return match result {
                    Some(obj) => Ok(obj),
                    None => Err(EvalError::UndefinedVarAccess { name: name.to_string() }),
                };
            }
            Not(ref expr) => {
                let b = self.eval(&*expr)?;
                return match b {
                    Object::Bool(b) => Ok(Object::Bool(!b)),
                    _ => unreachable!(),
                };
            }
            Tuple(ref tynds, ref _ty) => {
                let mut objs = Vec::new();
                for tynd in tynds.iter() {
                    let obj = self.eval(tynd)?;
                    objs.push(obj);
                }

                Ok(Object::Tuple(objs))
            }
            Expr { ref lhs, ref op, ref rhs, ref ty } => {
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
                            "<>" => lval == rval,
                            _ => unreachable!(),
                        }))
                    },
                    _ => unreachable!(),
                }
            }
            IfExpr {
                ref cond,
                ref then_body,
                ref else_body,
                ty: ref _ty,
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
            LetExpr {
                ref name,
                ref first_expr,
                ref second_expr,
                ty: ref _ty,
            } => {
                let result = self.eval(&*first_expr)?;
                let new_env = Environment::make_child((&self.env).clone());
                let env_rc = Rc::new(RefCell::new(new_env));
                let mut new_eval = Eval::from(env_rc.clone());
                let (ref id, ref _id_ty) = name;
                new_eval.bind(id.to_string(), result);
                return Ok(new_eval.eval(&*second_expr)?);
            }
            LetTupleExpr {
                ref names,
                ref first_expr,
                ref second_expr,
                tuple_ty: ref _tuple_ty,
                ty: ref _ty,
            } => {
                let result = self.eval(&*first_expr)?;
                let new_env = Environment::make_child((&self.env).clone());
                let env_rc = Rc::new(RefCell::new(new_env));
                let mut new_eval = Eval::from(env_rc.clone());
                let tuple_items = match result {
                    Object::Tuple(items) => {
                        items
                    },
                    _ => unreachable!(),
                };
                for ((name, _ty), item) in names.iter().zip(tuple_items.into_iter()) {
                    new_eval.bind(name.to_string(), item);
                }
                return Ok(new_eval.eval(&*second_expr)?);
            },
            MakeCls {
                ref name,
                ref actual_fv,
                ref second_expr,
                ty: ref _ty,
            } => {
                let new_env = Environment::make_child(self.env.clone());
                let closure_env = Rc::new(RefCell::new(new_env));
                let mut new_eval = Eval::from(closure_env.clone());
                
                let mut closure_fvs = Vec::new();
                for name in actual_fv.iter() {
                    closure_fvs.push((name.to_string(), self.env.borrow().get(name).unwrap()));
                }

                let (ref id, ref _id_ty) = name;

                let closure_obj = Object::Closure(Box::new(self.env.borrow().get(id).unwrap()), closure_fvs);
                new_eval.bind(id.to_string(), closure_obj);
                let result = new_eval.eval(&*second_expr)?;
                Ok(result)
            },
            // LetRecExpr {
            //     ref name,
            //     ref args,
            //     ref first_expr,
            //     ref second_expr,
            //     ty: ref _ty,
            // } => {
            //     let new_env = Environment::make_child((&self.env).clone());
            //     let func_env = Rc::new(RefCell::new(new_env));
            //     let mut new_eval = Eval::from(func_env.clone());
            //     let func_obj = Object::Func {
            //         body: first_expr.clone(),
            //         env: func_env.clone(),
            //         args: args.clone(),
            //     };
            //     let (ref id, ref _id_ty) = name;
            //     new_eval.bind(id.to_string(), func_obj);
            //     let result = new_eval.eval(&*second_expr)?;
            //     Ok(result)
            // }
            AppCls { ref func, ref args, func_ty: ref _func_ty, ty: ref _ty } => {
                let clos = self.eval(&*func)?;
                let mut new_func_env = Environment::new();
                let (func_obj, fvs) = match clos {
                    Object::Closure(func_obj, fvs) => (*func_obj, fvs),
                    _ => unreachable!(),
                };
                for (fv_name, fv_obj) in fvs.into_iter() {
                    new_func_env.set(fv_name, fv_obj);
                }
                let (func_body, arg_names) = match func_obj {
                    Object::Func { body, args } => {
                        (body, args.iter().map(|t| t.0.to_string()).collect::<Vec<String>>())
                    },
                    _ => unreachable!(),
                };
                for (arg_name, arg) in arg_names.iter().zip(args.iter()) {
                    new_func_env.set(arg_name.to_string(), self.eval(arg)?);
                }
                let env_rc = Rc::new(RefCell::new(new_func_env));
                let mut func_eval = Eval::from(env_rc);
                let result = func_eval.eval(&*func_body)?;
                return Ok(result);
                // if cnt >= func_args.len() {
                //     let mut func_eval = Eval::from(env_rc);
                //     let result = func_eval.eval(&*func_body)?;
                //     return Ok(result);
                // } else {
                //     return Ok(Object::Func {
                //         body: func_body,
                //         env: env_rc,
                //         args: func_args[cnt..].to_vec(),
                //     });
                // }
            },
            AppDir { ref func, ref args, func_ty: ref _func_ty, ty: ref _ty } => {
                let func_obj = self.env.borrow().get(func).unwrap();
                let (func_body, func_arg_names) = match func_obj {
                    Object::Func { body, args } => {
                        (body, args.iter().map(|t| t.0.to_string()).collect::<Vec<String>>())
                    },
                    _ => unreachable!(),
                };
                let mut func_env = Environment::new();
                for (arg_name, arg) in func_arg_names.iter().zip(args.iter()) {
                    func_env.set(arg_name.to_string(), self.eval(arg)?);
                }
                let env_rc = Rc::new(RefCell::new(func_env));
                let mut func_eval = Eval::from(env_rc);
                let result = func_eval.eval(&*func_body)?;
                return Ok(result);
            },
        }
    }
}

pub fn eval(prog: (Vec<FunDef>, CNode)) -> Result<Object, EvalError> {
    let mut e = Eval::new();
    e.eval_toplevel(prog.0);
    e.eval(&prog.1)
}
