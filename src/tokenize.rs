use combine::parser::char::*;
use combine::*;
use combine::stream::position;
use std::{process, unimplemented};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Num(i32),
    Op(String),
    LParen,
    RParen,
    Comma,
    If,
    Then,
    Else,
    Let,
    In,
    Rec,
    True,
    False,
    Not,
    Ident(String),
    Nop,
}

impl Token {
    pub fn get_string(&self) -> String {
        match *self {
            Token::Ident(ref s) => {
                s.to_string()
            },
            Token::Op(ref s) => {
                s.to_string()
            },
            _ => unimplemented!(),
        }
    }
}

parser! {
    fn num_token[Input]()(Input) -> Token
    where [
        Input: Stream<Token = char>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        many1::<String, _, _>(digit()).map(|s| Token::Num(s.parse::<i32>().unwrap()))
    }
}

parser! {
    fn op_token[Input]()(Input) -> Token
    where [
        Input: Stream<Token = char>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        choice((
            attempt((token('<'), token('='))).map(|_| Token::Op("<=".to_string())),
            attempt((token('>'), token('='))).map(|_| Token::Op(">=".to_string())),
            attempt((token('<'), token('>'))).map(|_| Token::Op("<>".to_string())),
            token('+').map(|_| Token::Op("+".to_string())),
            token('-').map(|_| Token::Op("-".to_string())),
            token('*').map(|_| Token::Op("*".to_string())),
            token('/').map(|_| Token::Op("/".to_string())),
            token('=').map(|_| Token::Op("=".to_string())),
            token('<').map(|_| Token::Op("<".to_string())),
            token('>').map(|_| Token::Op(">".to_string())),
        ))
    }
}

parser! {
    fn symbol_token[Input]()(Input) -> Token
    where [
        Input: Stream<Token = char>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        choice((
            token('(').map(|_| Token::LParen),
            token(')').map(|_| Token::RParen),
            token(',').map(|_| Token::Comma),
        ))
    }
}

parser! {
    fn keyword_token[Input]()(Input) -> Token
    where [
        Input: Stream<Token = char>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        many1::<String, _, _>(satisfy(|c: char| c.is_alphabetic() || c == '_' || c == '.')).map(|s: String| match s.as_str() {
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "let" => Token::Let,
            "in" => Token::In,
            "rec" => Token::Rec,
            "true" => Token::True,
            "false" => Token::False,
            "not" => Token::Not,
            _ => Token::Ident(s),
        })
    }
}

pub fn tokenize(s: &str) -> Vec<Token> {
    let mut lexer = many1::<Vec<Token>, _, _>(choice((
        num_token(),
        op_token(),
        symbol_token(),
        keyword_token(),
        skip_many1(satisfy(|c: char| c.is_whitespace())).map(|_| Token::Nop),
    )));
    let res = lexer.easy_parse(position::Stream::new(s));
    let tokens: Vec<Token> = match res {
        Ok(t) => t.0,
        Err(e) => {
            println!("{}", e);
            process::exit(1);
        }
    };
    tokens
        .into_iter()
        .filter_map(|tok: Token| match tok {
            Token::Nop => None,
            _ => Some(tok),
        })
        .collect()
}
