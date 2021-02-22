use super::tokenize::*;
use combine::error::ParseError;
use combine::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Int(i32),
    Bool(bool),
    VarExpr(String),
    Not(Box<Node>),
    Tuple(Vec<Node>),
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
    LetTupleExpr {
        names: Vec<String>,
        first_expr: Box<Node>,
        second_expr: Box<Node>,
    },
    LetRecExpr {
        name: String,
        args: Vec<String>,
        first_expr: Box<Node>,
        second_expr: Box<Node>,
    },
    App {
        func: Box<Node>,
        args: Vec<Node>,
    },
}

parser! {
    fn primary_expr[Input]()(Input) -> Node
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        choice((
            between(token(Token::LParen), token(Token::RParen), expr()),
            token(Token::True).map(|_| Node::Bool(true)),
            token(Token::False).map(|_| Node::Bool(false)),
            satisfy(|tok| match tok { Token::Ident(_) => true, _ => false })
                .map(|tok| match tok { Token::Ident(s) => Node::VarExpr(s), _ => unreachable!() }),
            satisfy(|tok| {
                match tok {
                    Token::Num(_) => true,
                    _ => false,
                }
            }).map(|tok| {
                match tok {
                    Token::Num(n) => Node::Int(n),
                    _ => unreachable!()
                }
            })
        ))
    }
}

parser! {
    fn unary_expr[Input]()(Input) -> Node
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        choice((
            primary_expr(),
            (
                token(Token::Not),
                unary_expr(),
            ).map(|t| Node::Not(Box::new(t.1))),
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
            unary_expr(),
            optional(many1::<Vec<(Token, Node)>, _, _>((
                choice((
                    token(Token::Op("*".to_string())), token(Token::Op("/".to_string()))
                )), unary_expr()
            )))
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
            optional(many1::<Vec<(Token, Node)>, _, _>((
                choice((
                    token(Token::Op("+".to_string())), token(Token::Op("-".to_string()))
                )), mul_expr()
            )))
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
    fn relational_expr[Input]()(Input) -> Node
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        (
            add_expr(),
            optional(many1::<Vec<(Token, Node)>, _, _>((
                choice((
                    token(Token::Op("<".to_string())), token(Token::Op("<=".to_string())),
                    token(Token::Op(">".to_string())), token(Token::Op(">=".to_string())),
                )), add_expr()
            )))
        ).map(|expr| {
            let mut left = expr.0;
            if let Some(x) = expr.1 {
                for (op_tok, node) in x.into_iter() {
                    let op = match op_tok {
                        Token::Op(s) => s,
                        _ => unreachable!()
                    };
                    match op.as_str() {
                        "<" => {
                            left = Node::Not(Box::new(Node::Expr {
                                lhs: Box::new(node),
                                op: String::from("<="),
                                rhs: Box::new(left),
                            }));
                        },
                        ">" => {
                            left = Node::Not(Box::new(Node::Expr {
                                lhs: Box::new(left),
                                op: String::from("<="),
                                rhs: Box::new(node),
                            }));
                        },
                        "<=" => {
                            left = Node::Expr {
                                lhs: Box::new(left),
                                op: String::from("<="),
                                rhs: Box::new(node),
                            };
                        },
                        ">=" => {
                            left = Node::Expr {
                                lhs: Box::new(node),
                                op: String::from("<="),
                                rhs: Box::new(left),
                            };
                        },
                        _ => unreachable!(),
                    }
                }
            }
            return left;
        })
    }
}

parser! {
    fn equal_expr[Input]()(Input) -> Node
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        (
            relational_expr(),
            optional(many1::<Vec<(Token, Node)>, _, _>((
                choice((
                    token(Token::Op("<>".to_string())),
                    token(Token::Op("=".to_string()))
                )),
                relational_expr()
            )))
        ).map(|expr| {
            let mut left = expr.0;
            if let Some(x) = expr.1 {
                for (op_tok, node) in x.into_iter() {
                    let op = match op_tok {
                        Token::Op(s) => s,
                        _ => unreachable!()
                    };
                    match op.as_str() {
                        "=" => {
                            left = Node::Expr {
                                lhs: Box::new(left),
                                op: "=".to_string(),
                                rhs: Box::new(node),
                            };
                        },
                        "<>" => {
                            left = Node::Not(Box::new(Node::Expr {
                                lhs: Box::new(left),
                                op: "=".to_string(),
                                rhs: Box::new(node),
                            }));
                        },
                        _ => unreachable!(),
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
    fn ident[Input]()(Input) -> Token
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        satisfy(|tok| match tok { Token::Ident(_) => true, _ => false })
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
            ident(),
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
    fn tuple_ids[Input]()(Input) -> Vec<String>
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        (
            ident(),
            token(Token::Comma)
            .with(ident()),
            optional(many1::<Vec<Token>, _, _>(token(Token::Comma).with(ident()))),
        ).map(|(id1, id2, ids)| {
            let mut id_vec = Vec::new();
            id_vec.push(id1.get_string());
            id_vec.push(id2.get_string());
            if let Some(ids) = ids {
                id_vec.extend(ids.into_iter().map(|t| t.get_string()));
            }

            id_vec
        })
    }
}

parser! {
    fn lettuple_expr[Input]()(Input) -> Node
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        (
            token(Token::Let),
            between(token(Token::LParen), token(Token::RParen), tuple_ids()),
            token(Token::Op("=".to_string())),
            expr(),
            token(Token::In),
            expr(),
        ).map(|let_expr| {
            let names = let_expr.1;
            Node::LetTupleExpr {
                names,
                first_expr: Box::new(let_expr.3),
                second_expr: Box::new(let_expr.5),
            }
        })
    }
}

parser! {
    fn formal_args[Input]()(Input) -> Vec<String>
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        many1::<Vec<String>, _, _>(ident().map(|tok| match tok { Token::Ident(s) => s, _ => unreachable!() }))
    }
}

parser! {
    fn func_def[Input]()(Input) -> (String, Vec<String>, Node)
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        (
            ident(),
            formal_args(),
            token(Token::Op("=".to_string())),
            expr(),
        ).map(|t| {
            let name = match t.0 { Token::Ident(s) => s, _ => unreachable!() };
            (name, t.1, t.3)
        })
    }
}

parser! {
    fn letrec_expr[Input]()(Input) -> Node
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        (
            token(Token::Let),
            token(Token::Rec),
            func_def(),
            token(Token::In),
            expr(),
        ).map(|t| {
            let name = t.2.0;
            let args_ = t.2.1;
            let mut args: Vec<String> = Vec::new();
            for name in args_.into_iter() {
                args.push(name);
            }
            let first_expr = t.2.2;
            Node::LetRecExpr {
                name,
                args,
                first_expr: Box::new(first_expr),
                second_expr: Box::new(t.4),
            }
        })
    }
}

parser! {
    fn actual_arg[Input]()(Input) -> Vec<Node>
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        many1::<Vec<Node>, _, _>(primary_expr())
    }
}

parser! {
    fn app_expr[Input]()(Input) -> Node
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        (
            primary_expr(),
            actual_arg(),
        ).map(|t| {
            Node::App {
                func: Box::new(t.0),
                args: t.1,
            }
        })
    }
}

parser! {
    fn tuple_expr[Input]()(Input) -> Node
    where [
        Input: Stream<Token = Token>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        (
            token(Token::LParen),
            expr(),
            token(Token::Comma),
            expr(),
            optional(many1::<Vec<Node>, _, _>((token(Token::Comma), expr()).map(|t| t.1))),
            token(Token::RParen),
        ).map(|(_, e1, _, e2, es, _)| {
            let mut tuple_vec = Vec::new();
            tuple_vec.push(e1);
            tuple_vec.push(e2);
            if let Some(es) = es {
                tuple_vec.extend(es);
            }
            Node::Tuple(tuple_vec)
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
            attempt(app_expr()),
            attempt(tuple_expr()),
            attempt(equal_expr()),
            attempt(if_expr()),
            attempt(lettuple_expr()),
            attempt(let_expr()),
            attempt(letrec_expr()),
        ))
    }
}

pub fn parse(tokens: Vec<Token>) -> Node {
    let mut parser = expr();
    parser.parse(&tokens[..]).unwrap().0
}
