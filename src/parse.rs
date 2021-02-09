use super::tokenize::*;
use combine::error::ParseError;
use combine::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Num(i32),
    VarExpr(String),
    Expr {
        lhs: Box<Node>,
        op: String,
        rhs: Box<Node>,
    },
    IfExpr {
        cond: Box<Node>,
        then_body: Box<Node>,
        else_body: Box<Node>,
    },
    LetExpr {
        name: String,
        first_expr: Box<Node>,
        second_expr: Box<Node>,
    },
}

parser! {
    fn primary_expr[Input]()(Input) -> Node
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        choice((
            between(token(Token::LParen), token(Token::RParen), add_expr()),
            satisfy(|tok| match tok { Token::Ident(_) => true, _ => false })
                .map(|tok| match tok { Token::Ident(s) => Node::VarExpr(s), _ => unreachable!() }),
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
        ))
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

parser! {
    fn if_expr[Input]()(Input) -> Node
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        (
            token(Token::If),
            expr(), // cond
            token(Token::Then),
            expr(), // then_body
            token(Token::Else),
            expr(), // else_body
        ).map(|if_expr| {
            Node::IfExpr {
                cond: Box::new(if_expr.1),
                then_body: Box::new(if_expr.3),
                else_body: Box::new(if_expr.5),
            }
        })
    }
}

parser! {
    fn let_expr[Input]()(Input) -> Node
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        (
            token(Token::Let),
            satisfy(|tok| match tok { Token::Ident(_) => true, _ => false }),
            token(Token::Op("=".to_string())),
            expr(),
            token(Token::In),
            expr(),
        ).map(|let_expr| {
            let name = match let_expr.1 { Token::Ident(s) => s, _ => unreachable!() };
            Node::LetExpr {
                name,
                first_expr: Box::new(let_expr.3),
                second_expr: Box::new(let_expr.5),
            }
        })
    }
}

parser! {
    fn expr[Input]()(Input) -> Node
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        choice((
            add_expr(),
            if_expr(),
            let_expr(),
        ))
    }
}

pub fn parse(tokens: Vec<Token>) -> Node {
    let mut parser = expr();
    parser.parse(&tokens[..]).unwrap().0
}
