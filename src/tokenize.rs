use std::fmt;
use std::{process, unimplemented};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Num(i32),
    Float(f64),
    Op(String),
    LParen,
    RParen,
    Comma,
    SemiColon,
    Dot,
    If,
    Then,
    Else,
    Let,
    In,
    Rec,
    True,
    False,
    Array,
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
            Dot => write!(f, "Dot"),
            If => write!(f, "If"),
            Then => write!(f, "Then"),
            Else => write!(f, "Else"),
            Let => write!(f, "Let"),
            In => write!(f, "In"),
            Rec => write!(f, "Rec"),
            True => write!(f, "True"),
            False => write!(f, "False"),
            Array => write!(f, "Array"),
            Ident(ref s) => write!(f, "Ident({})", s),
            Nop => write!(f, "Nop"),
            Eof => write!(f, "Eof"),
        }
    }
}

impl Token {
    pub fn get_string(&self) -> String {
        match *self {
            Token::Ident(ref s) => s.to_string(),
            Token::Op(ref s) => s.to_string(),
            _ => unimplemented!(),
        }
    }
}

pub struct Tokenizer{
    input: Vec<char>,
    pos: usize,
}

impl Tokenizer {
    pub fn new(input: &str) -> Tokenizer {
        Tokenizer {
            input: input.chars().collect(),
            pos: 0,
        }
    }

    pub fn read(&mut self) -> Option<char> {
        self.pos += 1;
        match self.input.get(self.pos - 1) {
            Some(ch) => Some(*ch),
            None => None,
        }
    }

    pub fn peek(&self, offset: usize) -> Option<char> {
        match self.input.get(self.pos + offset) {
            Some(ch) => Some(*ch),
            None => None,
        }
    }

    pub fn curr(&self) -> Option<char> {
        self.peek(0)
    }

    pub fn skip(&mut self, n: usize) {
        self.pos += n;
    }

    pub fn read_seq(&mut self, seq: Vec<char>) -> bool {
        let seq_len = seq.len();
        for (i, ch) in seq.into_iter().enumerate() {
            if self.peek(i) != Some(ch) {
                return false;
            }
        }

        self.skip(seq_len);
        return true;
    }

    pub fn get_curr_pos(&self) -> usize {
        self.pos
    }

    pub fn read_while<F: Fn(char) -> bool>(&mut self, f: F) -> (usize, usize) {
        let begin = self.get_curr_pos();
        while let Some(ch) = self.curr() {
            if !f(ch) {
                break;
            }
            self.skip(1);
        }
        let end = self.get_curr_pos();
        (begin, end)
    }

    pub fn next(&mut self) -> Token {
        while let Some(ch) = self.curr() {
            if ch.is_whitespace() {
                self.skip(1);
                continue;
            }
            if ch == '(' && self.peek(1) == Some('*') {
                self.skip(2);
                let mut level: usize = 1;
                while let Some(ch) = self.curr() {
                    if ch == '(' && self.peek(1) == Some('*') {
                        self.skip(2);
                        level += 1;
                        continue;
                    }
                    if ch == '*' && self.peek(1) == Some(')') {
                        self.skip(2);
                        level -= 1;
                        if level == 0 {
                            break;
                        }
                        continue;
                    }
                    self.skip(1);
                }
                if level != 0 {
                    println!("Comment not closed");
                    process::exit(1);
                }
                continue;
            }
            if ch.is_alphabetic() || ch == '_' {
                let (begin, end) = self.read_while(|ch| ch.is_alphanumeric() || ch == '_');
                let t: String = self.input[begin..end].iter().collect();
                let tok = match t.as_str() {
                    "if" => Token::If,
                    "then" => Token::Then,
                    "else" => Token::Else,
                    "let" => Token::Let,
                    "in" => Token::In,
                    "rec" => Token::Rec,
                    "true" => Token::True,
                    "false" => Token::False,
                    "Array" => Token::Array,
                    "not" => Token::Op("not".to_string()),
                    _ => Token::Ident(t),
                };
                
                return tok;
            }

            if ch.is_ascii_digit() {
                let (begin, end) = self.read_while(|ch| ch.is_ascii_digit());
                let int_part: String = self.input[begin..end].iter().collect();
                if self.curr() == Some('.') || self.curr() == Some('e') || self.curr() == Some('E') {
                    let mut sep = "";
                    let dec_part = if self.curr() == Some('.') {
                        sep = ".";
                        self.skip(1);
                        let (begin, end) = self.read_while(|ch| ch.is_ascii_digit());
                        self.input[begin..end].iter().collect()
                    } else {
                        "".to_string()
                    };

                    let exp_part = if self.curr() == Some('e') || self.curr() == Some('E') {
                        self.skip(1);
                        let prefix = if self.curr() == Some('+') || self.curr() == Some('-') {
                            self.read().unwrap()
                        } else {
                            '+'
                        };
                        let (begin, end) = self.read_while(|ch| ch.is_ascii_digit());
                        let exp: String = self.input[begin..end].iter().collect();
                        format!("e{}{}", prefix, exp)
                    } else {
                        "".to_string()
                    };
                    let float_val = format!("{}{}{}{}", int_part, sep, dec_part, exp_part).parse::<f64>().unwrap();
                    
                    return Token::Float(float_val);
                } else {
                    let int_val = int_part.parse::<i32>().unwrap();

                    return Token::Num(int_val);
                }
            }

            if self.read_seq(vec!['<', '-']) {
                return Token::Op("<-".to_string());
            }
            if self.read_seq(vec!['<', '=']) {
                return Token::Op("<=".to_string());
            }
            if self.read_seq(vec!['>', '=']) {
                return Token::Op(">=".to_string());
            }
            if self.read_seq(vec!['<', '>']) {
                return Token::Op("<>".to_string());
            }
            if self.read_seq(vec!['+', '.']) {
                return Token::Op("+.".to_string());
            }
            if self.read_seq(vec!['-', '.']) {
                return Token::Op("-.".to_string());
            }
            if self.read_seq(vec!['*', '.']) {
                return Token::Op("*.".to_string());
            }
            if self.read_seq(vec!['/', '.']) {
                return Token::Op("/.".to_string());
            }

            self.skip(1);
            if ch == '+' {
                return Token::Op("+".to_string());
            }
            if ch == '-' {
                return Token::Op("-".to_string());
            }
            if ch == '*' {
                return Token::Op("*".to_string());
            }
            if ch == '/' {
                return Token::Op("/".to_string());
            }
            if ch == '=' {
                return Token::Op("=".to_string());
            }
            if ch == '<' {
                return Token::Op("<".to_string());
            }
            if ch == '>' {
                return Token::Op(">".to_string());
            }
            if ch == '.' {
                return Token::Dot;
            }
            if ch == '(' {
                return Token::LParen;
            }
            if ch == ')' {
                return Token::RParen;
            }
            if ch == ',' {
                return Token::Comma;
            }
            if ch == ';' {
                return Token::SemiColon;
            }


            panic!("Unknown character: {}", ch);
        }

        return Token::Eof;
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut res = Vec::new();
        let mut tok = self.next();
        loop {
            match tok {
                Token::Eof => break,
                _ => res.push(tok),
            }
            tok = self.next();
        }

        res
    }
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut t = Tokenizer::new(input);
    t.tokenize()
}

#[test]
fn tokenizer_test1() {
    let mut t = Tokenizer::new("1.0e-2");
    let tokens = t.tokenize();
    assert_eq!(tokens[0], Token::Float(0.01));
}

// parser! {
//     fn num_token[Input]()(Input) -> Token
//     where [
//         Input: Stream<Token = char>,
//         Input::Error: ParseError<char, Input::Range, Input::Position>,
//     ] {
//         many1::<String, _, _>(digit()).map(|s| Token::Num(s.parse::<i32>().unwrap()))
//     }
// }
// 
// parser! {
//     fn float_num_token[Input]()(Input) -> Token
//     where [
//         Input: Stream<Token = char>,
//         Input::Error: ParseError<char, Input::Range, Input::Position>,
//     ] {
//         (
//             many1::<String, _, _>(digit()),
//             token('.'),
//             many::<String, _, _>(digit()),
//         ).map(|(int, _, dec)| Token::Float(format!("{}.{}", int, dec).parse::<f32>().unwrap()))
//     }
// }
// 
// parser! {
//     fn op_token[Input]()(Input) -> Token
//     where [
//         Input: Stream<Token = char>,
//         Input::Error: ParseError<char, Input::Range, Input::Position>,
//     ] {
//         choice((
//             attempt((token('<'), token('-'))).map(|_| Token::Op("<-".to_string())),
//             attempt((token('<'), token('='))).map(|_| Token::Op("<=".to_string())),
//             attempt((token('>'), token('='))).map(|_| Token::Op(">=".to_string())),
//             attempt((token('<'), token('>'))).map(|_| Token::Op("<>".to_string())),
//             attempt((token('+'), token('.'))).map(|_| Token::Op("+.".to_string())),
//             attempt((token('-'), token('.'))).map(|_| Token::Op("-.".to_string())),
//             attempt((token('*'), token('.'))).map(|_| Token::Op("*.".to_string())),
//             attempt((token('/'), token('.'))).map(|_| Token::Op("/.".to_string())),
//             token('+').map(|_| Token::Op("+".to_string())),
//             token('-').map(|_| Token::Op("-".to_string())),
//             token('*').map(|_| Token::Op("*".to_string())),
//             token('/').map(|_| Token::Op("/".to_string())),
//             token('=').map(|_| Token::Op("=".to_string())),
//             token('<').map(|_| Token::Op("<".to_string())),
//             token('>').map(|_| Token::Op(">".to_string())),
//             token('.').map(|_| Token::Dot),
//         ))
//     }
// }
// 
// parser! {
//     fn symbol_token[Input]()(Input) -> Token
//     where [
//         Input: Stream<Token = char>,
//         Input::Error: ParseError<char, Input::Range, Input::Position>,
//     ] {
//         choice((
//             token('(').map(|_| Token::LParen),
//             token(')').map(|_| Token::RParen),
//             token(',').map(|_| Token::Comma),
//             token(';').map(|_| Token::SemiColon),
//         ))
//     }
// }
// 
// parser! {
//     fn keyword_token[Input]()(Input) -> Token
//     where [
//         Input: Stream<Token = char>,
//         Input::Error: ParseError<char, Input::Range, Input::Position>,
//     ] {
//         (
//             many1::<String, _, _>(satisfy(|c: char| c.is_alphabetic() || c == '_')),
//             many::<String, _, _>(satisfy(|c: char| c.is_alphanumeric() || c == '_'))
//         ).map(|(s, t)| match format!("{}{}", s, t).as_str() {
//             "if" => Token::If,
//             "then" => Token::Then,
//             "else" => Token::Else,
//             "let" => Token::Let,
//             "in" => Token::In,
//             "rec" => Token::Rec,
//             "true" => Token::True,
//             "false" => Token::False,
//             "Array" => {
//                 Token::Array
//             },
//             "not" => Token::Op("not".to_string()),
//             _ => Token::Ident(format!("{}{}", s, t)),
//         })
//     }
// }
// 
// parser! {
//     fn comment[Input]()(Input) -> Token
//     where [
//         Input: Stream<Token = char>,
//         Input::Error: ParseError<char, Input::Range, Input::Position>,
//     ] {
//         // between((token('('), token('*')), (token('*'), token(')')), many(any())).map(|_: String| Token::Nop)
//         (
//             token('('),
//             token('*'),
//             take_until(attempt((token('*'), token(')')))),
//             token('*'),
//             token(')'),
//         ).map(|_: (char, char, String, char, char)| Token::Nop)
//     }
// }
// 
// pub fn tokenize(s: &str) -> Vec<Token> {
//     println!("Tokenize");
//     let mut lexer = many1::<Vec<Token>, _, _>(choice((
//         attempt(comment()),
//         attempt(float_num_token()),
//         attempt(num_token()),
//         op_token(),
//         symbol_token(),
//         keyword_token(),
//         skip_many1(satisfy(|c: char| c.is_whitespace())).map(|_| Token::Nop),
//     )));
//     let res = lexer.easy_parse(position::Stream::new(s));
//     let tokens: Vec<Token> = match res {
//         Ok(t) => t.0,
//         Err(e) => {
//             println!("{}", e);
//             process::exit(1);
//         }
//     };
//     tokens
//         .into_iter()
//         .filter_map(|tok: Token| match tok {
//             Token::Nop => None,
//             _ => Some(tok),
//         })
//         .collect()
// }
