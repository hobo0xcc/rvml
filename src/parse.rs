use combine::error::ParseError;
use combine::*;
use super::tokenize::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Num(i32),
    Expr {
        lhs: Box<Node>,
        op: String,
        rhs: Box<Node>,
    },
}

parser! {
    fn primary_expr[Input]()(Input) -> Node
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        satisfy(|tok| {
            match tok {
                Token::Num(_) => true,
                _ => false,
            }
        }).map(|tok| {
            match tok {
                Token::Num(n) => Node::Num(n),
                _ => unreachable!()
            }
        })
    }
}

parser! {
    fn mul_expr[Input]()(Input) -> Node
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        (
            primary_expr(),
            optional(many1::<Vec<(Token, Node)>, _, _>((choice((token(Token::Op("*".to_string())), token(Token::Op("/".to_string())))), primary_expr())))
        ).map(|expr| {
            let mut left = expr.0;
            if let Some(x) = expr.1 {
                for (op_tok, node) in x.into_iter() {
                    let op = match op_tok {
                        Token::Op(s) => s,
                        _ => unreachable!()
                    };
                    left = Node::Expr {
                        lhs: Box::new(left),
                        op,
                        rhs: Box::new(node),
                    };
                }
            }
            return left;
        })
    }
}

parser! {
    fn add_expr[Input]()(Input) -> Node
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        (
            mul_expr(),
            optional(many1::<Vec<(Token, Node)>, _, _>((choice((token(Token::Op("+".to_string())), token(Token::Op("-".to_string())))), mul_expr())))
        ).map(|expr| {
            let mut left = expr.0;
            if let Some(x) = expr.1 {
                for (op_tok, node) in x.into_iter() {
                    let op = match op_tok {
                        Token::Op(s) => s,
                        _ => unreachable!()
                    };
                    left = Node::Expr {
                        lhs: Box::new(left),
                        op,
                        rhs: Box::new(node),
                    };
                }
            }
            return left;
        })
    }
}

pub fn parse(tokens: Vec<Token>) -> Node {
    let mut parser = add_expr();
    parser.parse(&tokens[..]).unwrap().0
}