use combine::parser::char::*;
use combine::*;
use combine::stream::position;
use std::{process, unimplemented};
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Num(i32),
    Float(f32),
    Op(String),
    LParen,
    RParen,
    Comma,
    SemiColon,
    If,
    Then,
    Else,
    Let,
    In,
    Rec,
    True,
    False,
    Ident(String),
    Nop,
    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Token::*;
        match *self {
            Num(ref n) => write!(f, "Num({})", *n),
            Float(ref fl) => write!(f, "Float({})", *fl),
            Op(ref s) => write!(f, "Op({})", s),
            LParen => write!(f, "LParen"),
            RParen => write!(f, "RParen"),
            Comma => write!(f, "Comma"),
            SemiColon => write!(f, "SemiColon"),
            If => write!(f, "If"),
            Then => write!(f, "Then"),
            Else => write!(f, "Else"),
            Let => write!(f, "Let"),
            In => write!(f, "In"),
            Rec => write!(f, "Rec"),
            True => write!(f, "True"),
            False => write!(f, "False"),
            Ident(ref s) => write!(f, "Ident({})", s),
            Nop => write!(f, "Nop"),
            Eof => write!(f, "Eof"),
        }
    }
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
    fn float_num_token[Input]()(Input) -> Token
    where [
        Input: Stream<Token = char>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        (
            many1::<String, _, _>(digit()),
            token('.'),
            digit(),
        ).map(|(int, _, dec)| Token::Float(format!("{}.{}", int, dec).parse::<f32>().unwrap()))
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
            attempt((token('+'), token('.'))).map(|_| Token::Op("+.".to_string())),
            attempt((token('-'), token('.'))).map(|_| Token::Op("-.".to_string())),
            attempt((token('*'), token('.'))).map(|_| Token::Op("*.".to_string())),
            attempt((token('/'), token('.'))).map(|_| Token::Op("/.".to_string())),
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
            token(';').map(|_| Token::SemiColon),
        ))
    }
}

parser! {
    fn keyword_token[Input]()(Input) -> Token
    where [
        Input: Stream<Token = char>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        (
            many1::<String, _, _>(satisfy(|c: char| c.is_alphabetic() || c == '_' || c == '.')),
            many::<String, _, _>(satisfy(|c: char| c.is_alphanumeric() || c == '_' || c == '.'))
        ).map(|(s, t)| match format!("{}{}", s, t).as_str() {
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "let" => Token::Let,
            "in" => Token::In,
            "rec" => Token::Rec,
            "true" => Token::True,
            "false" => Token::False,
            "not" => Token::Op("not".to_string()),
            _ => Token::Ident(format!("{}{}", s, t)),
        })
    }
}

pub fn tokenize(s: &str) -> Vec<Token> {
    println!("Tokenize");
    let mut lexer = many1::<Vec<Token>, _, _>(choice((
        attempt(float_num_token()),
        attempt(num_token()),
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
