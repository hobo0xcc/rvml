use super::tokenize::*;
use rpds::Vector;
use std::process;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Unit,
    Int(i32),
    Float(f32),
    Bool(bool),
    VarExpr(String),
    Not(Box<Node>),
    Tuple(Vector<Node>),
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
        names: Vector<String>,
        first_expr: Box<Node>,
        second_expr: Box<Node>,
    },
    LetRecExpr {
        name: String,
        args: Vector<String>,
        first_expr: Box<Node>,
        second_expr: Box<Node>,
    },
    App {
        func: Box<Node>,
        args: Vector<Node>,
    },
}

pub struct Parser {
    tokens: Vec<Token>,
    curr: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, curr: 0 }
    }

    pub fn peek(&self, offset: usize) -> Token {
        match self.tokens.get(self.curr + offset) {
            Some(tok) => tok.clone(),
            None => Token::Eof,
        }
    }

    pub fn curr(&self) -> Token {
        self.peek(0)
    }

    pub fn read(&mut self) -> Token {
        let curr_tok = self.curr();

        self.curr += 1;
        curr_tok
    }

    pub fn read_if<F: Fn(&Token) -> bool>(&mut self, f: F) -> Option<Token> {
        let curr_tok = self.curr();
        if f(&curr_tok) {
            self.next();
            Some(curr_tok)
        } else {
            None
        }
    }

    pub fn next(&mut self) {
        let _ = self.read();
    }

    pub fn expect(&mut self, exp_tok: &Token) {
        let curr_tok = self.read();
        if curr_tok != *exp_tok {
            println!("Error: Expected {}, found {}", exp_tok, curr_tok);
            process::exit(1);
        }
    }

    pub fn expect_fn<F: Fn(&Token) -> bool>(&mut self, f: F) {
        let curr_tok = self.read();
        if !f(&curr_tok) {
            println!("Error: Unexpected token: {}", curr_tok);
            process::exit(1);
        }
    }

    pub fn prefix_bp(&self, op: &str) -> ((), usize) {
        match op {
            "not" => ((), 9),
            "if" => ((), 5),
            "let" => ((), 1),
            _ => {
                println!("Bad op: {}", op);
                process::exit(1);
            }
        }
    }

    pub fn infix_bp(&self, op: &str) -> Option<(usize, usize)> {
        let res = match op {
            ";" => (4, 3),
            "," => (7, 8),
            "<" | ">" | "<=" | ">=" | "=" | "<>" => (9, 10),
            "+" | "-" | "+." | "-." => (11, 12),
            "*" | "/" | "*." | "/." => (13, 14),
            _ => return None,
        };
        Some(res)
    }

    pub fn postfix_bp(&self, _op: &str) -> Option<(usize, ())> {
        None
        // let res = match op {
        //     _ => return None,
        // };

        // res
    }

    pub fn formal_args(&mut self) -> Vector<String> {
        let is_ident = |tok: &Token| match *tok {
            Token::Ident(_) => true,
            _ => false,
        };
        let mut args = Vector::new();
        while let Some(tok) = self.read_if(is_ident) {
            let name = match tok {
                Token::Ident(s) => s,
                _ => unreachable!(),
            };
            args = args.push_back(name);
        }

        if args.len() == 0 {
            println!("Expected args, found {}", self.curr());
            process::exit(1);
        }

        args
    }

    pub fn actual_args(&mut self) -> Option<Vector<Node>> {
        let mut args = Vector::new();
        while let Some(nd) = self.simple_expr() {
            args = args.push_back(nd);
        }

        if args.len() == 0 {
            return None;
        }

        Some(args)
    }

    pub fn tuple_ids(&mut self) -> Vector<String> {
        self.expect(&Token::LParen);
        let is_ident = |tok: &Token| match *tok {
            Token::Ident(_) => true,
            _ => false,
        };
        let mut ids = Vector::new();
        while let Some(tok) = self.read_if(is_ident) {
            let name = match tok {
                Token::Ident(s) => s,
                _ => unreachable!(),
            };
            ids = ids.push_back(name);
            match self.curr() {
                Token::Comma => {
                    self.next();
                }
                _ => break,
            }
        }

        self.expect(&Token::RParen);

        if ids.len() < 2 {
            println!("Tuple size must be greater than 1");
            process::exit(1);
        }

        ids
    }

    pub fn simple_expr(&mut self) -> Option<Node> {
        let res = match self.curr() {
            Token::Num(n) => Node::Int(n),
            Token::Float(f) => Node::Float(f),
            Token::Ident(name) => Node::VarExpr(name),
            Token::True => Node::Bool(true),
            Token::False => Node::Bool(false),
            Token::LParen => {
                self.next();
                match self.curr() {
                    Token::RParen => {
                        self.next();
                        return Some(Node::Unit);
                    }
                    _ => {
                        let e = self.expr(0);
                        self.expect(&Token::RParen);
                        return Some(e);
                    }
                }
            }
            _ => return None,
        };

        self.next();
        Some(res)
    }

    pub fn expr(&mut self, bp: usize) -> Node {
        let mut lhs = match self.curr() {
            Token::Op(op) => {
                self.next();
                let ((), r_bp) = self.prefix_bp(&op);
                let rhs = self.expr(r_bp);
                match op.as_str() {
                    "not" => Node::Not(Box::new(rhs)),
                    _ => unreachable!(),
                }
            }
            Token::If => {
                self.next();
                let ((), r_bp) = self.prefix_bp("if");
                let cond = self.expr(0);
                self.expect(&Token::Then);
                let e1 = self.expr(0);
                self.expect(&Token::Else);
                let e2 = self.expr(r_bp);

                Node::IfExpr {
                    cond: Box::new(cond),
                    then_body: Box::new(e1),
                    else_body: Box::new(e2),
                }
            }
            Token::Let => {
                self.next();
                let ((), r_bp) = self.prefix_bp("let");
                match self.curr() {
                    Token::Rec => {
                        self.next();
                        let name = match self.read() {
                            Token::Ident(name) => name,
                            tok => {
                                println!("Expected Ident, found {}", tok);
                                process::exit(1);
                            }
                        };

                        let args = self.formal_args();
                        self.expect(&Token::Op("=".to_string()));
                        let e1 = self.expr(0);
                        self.expect(&Token::In);
                        let e2 = self.expr(r_bp);

                        return Node::LetRecExpr {
                            name,
                            args,
                            first_expr: Box::new(e1),
                            second_expr: Box::new(e2),
                        };
                    }
                    Token::LParen => {
                        let ids = self.tuple_ids();
                        self.expect(&Token::Op("=".to_string()));
                        let e1 = self.expr(0);
                        self.expect(&Token::In);
                        let e2 = self.expr(r_bp);

                        return Node::LetTupleExpr {
                            names: ids,
                            first_expr: Box::new(e1),
                            second_expr: Box::new(e2),
                        };
                    }
                    _ => {
                        let name = match self.read() {
                            Token::Ident(s) => s,
                            tok => {
                                println!("Expected Ident, found {}", tok);
                                process::exit(1);
                            }
                        };

                        self.expect(&Token::Op("=".to_string()));
                        let e1 = self.expr(0);
                        self.expect(&Token::In);
                        let e2 = self.expr(r_bp);

                        return Node::LetExpr {
                            name,
                            first_expr: Box::new(e1),
                            second_expr: Box::new(e2),
                        };
                    }
                }
            }
            _ => match self.simple_expr() {
                Some(nd) => {
                    if let Some(args) = self.actual_args() {
                        Node::App {
                            func: Box::new(nd),
                            args,
                        }
                    } else {
                        nd
                    }
                }
                None => {
                    println!("Bad token: {}", self.curr());
                    process::exit(1);
                }
            },
        };

        loop {
            let op = match self.curr() {
                Token::Eof => break,
                Token::Then => break,
                Token::Else => break,
                Token::RParen => break,
                Token::In => break,
                Token::Op(op) => op,
                Token::Comma => ",".to_string(),
                Token::SemiColon => ";".to_string(),
                tok => {
                    println!("Bad token: {}", tok);
                    process::exit(1);
                }
            };

            if let Some((l_bp, ())) = self.postfix_bp(&op) {
                if l_bp < bp {
                    break;
                }
                self.next();

                continue;
            }

            if let Some((l_bp, r_bp)) = self.infix_bp(&op) {
                if l_bp < bp {
                    break;
                }
                self.next();

                let rhs = self.expr(r_bp);
                match op.as_str() {
                    "," => {
                        let elems = match lhs {
                            Node::Tuple(ref old_elems) => old_elems.push_back(rhs),
                            _ => {
                                let mut new_elems = Vector::new();
                                new_elems = new_elems.push_back(lhs);
                                new_elems.push_back(rhs)
                            }
                        };
                        lhs = Node::Tuple(elems);
                    }
                    ";" => {
                        lhs = Node::LetExpr {
                            name: "_".to_string(),
                            first_expr: Box::new(lhs),
                            second_expr: Box::new(rhs),
                        };
                    }
                    _ => {
                        lhs = Node::Expr {
                            lhs: Box::new(lhs),
                            op: op.to_string(),
                            rhs: Box::new(rhs),
                        };
                    }
                }

                continue;
            }

            break;
        }

        lhs
    }
}

pub fn parse(tokens: Vec<Token>) -> Node {
    println!("Parse");
    let mut parser = Parser::new(tokens);
    parser.expr(0)
}

#[test]
fn parse_test1() {
    let node = parse(tokenize("1"));
    assert_eq!(node, Node::Int(1));
}
