use super::parse::*;

pub fn eval(node: Node) -> i32 {
    match node {
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
        Node::Num(n) => {
            return n;
        }
    }
}