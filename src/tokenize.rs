use combine::parser::char::*;
use combine::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Num(i32),
    Op(String),
    LParen,
    RParen,
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
            token('+').map(|_| Token::Op("+".to_string())),
            token('-').map(|_| Token::Op("-".to_string())),
            token('*').map(|_| Token::Op("*".to_string())),
            token('/').map(|_| Token::Op("/".to_string())),
            token('=').map(|_| Token::Op("=".to_string())),
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
        ))
    }
}

parser! {
    fn keyword_token[Input]()(Input) -> Token
    where [
        Input: Stream<Token = char>,
        Input::Error: ParseError<char, Input::Range, Input::Position>,
    ] {
        many1::<String, _, _>(satisfy(|c: char| c.is_alphabetic())).map(|s: String| match s.as_str() {
            "if" => Token::If,
            "then" => Token::Then,
            "else" => Token::Else,
            "let" => Token::Let,
            "in" => Token::In,
            "rec" => Token::Rec,
            "true" => Token::True,
            "false" => Token::False,
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
    let tokens: Vec<Token> = lexer.easy_parse(s).unwrap().0;
    tokens
        .into_iter()
        .filter_map(|tok: Token| match tok {
            Token::Nop => None,
            _ => Some(tok),
        })
        .collect()
}
