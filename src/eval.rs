use super::parse::*;

pub fn eval(node: Node) -> i32 {
    match node {
        Node::Num(n) => {
            return n;
        },
        Node::Expr { lhs, op, rhs } => {
            let lval = eval(*lhs);
            let rval = eval(*rhs);
            match op.as_str() {
                "+" => lval + rval,
                "-" => lval - rval,
                "*" => lval * rval,
                "/" => lval * rval,
                _ => unreachable!()
            }
        },
        Node::IfExpr { cond, then_body, else_body } => {
            if eval(*cond) != 0 {
                return eval(*then_body);
            } else {
                return eval(*else_body);
            }
        },
    }
}