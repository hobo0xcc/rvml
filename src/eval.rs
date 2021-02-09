use super::parse::*;
use std::collections::HashMap;

pub struct EvalEnv {
    local_vars: HashMap<String, i32>,
}

impl EvalEnv {
    pub fn new() -> EvalEnv {
        EvalEnv {
            local_vars: HashMap::new(),
        }
    }
}

pub fn eval(node: Node, env: &mut EvalEnv) -> i32 {
    match node {
        Node::Num(n) => {
            return n;
        },
        Node::VarExpr(name) => {
            return *env.local_vars.get(&name).unwrap();
        },
        Node::Expr { lhs, op, rhs } => {
            let lval = eval(*lhs, env);
            let rval = eval(*rhs, env);
            match op.as_str() {
                "+" => lval + rval,
                "-" => lval - rval,
                "*" => lval * rval,
                "/" => lval * rval,
                _ => unreachable!()
            }
        },
        Node::IfExpr { cond, then_body, else_body } => {
            if eval(*cond, env) != 0 {
                return eval(*then_body, env);
            } else {
                return eval(*else_body, env);
            }
        },
        Node::LetExpr { name, first_expr, second_expr } => {
            let result = eval(*first_expr, env);
            env.local_vars.insert(name, result);
            return eval(*second_expr, env);
        }
    }
}