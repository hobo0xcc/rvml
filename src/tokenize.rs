use combine::parser::char::*;
use combine::*;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Num(i32),
    Op(String),
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
        ))
    }
}

pub fn tokenize(s: &str) -> Vec<Token> {
    let mut lexer = many1::<Vec<Token>, _, _>(choice((
        num_token(),
        op_token(),
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