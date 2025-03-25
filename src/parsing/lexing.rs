use std::cmp::min;

use regex_automata::meta::Regex;
use regex_automata::{Anchored, Input};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TokenType {
    LineComment,
    Return,
    Fn,
    If,
    Else,
    Identifier,
    NumberLit,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Let,
    Semi,
    EqAssign,
    True,
    False,
    Bang,
    And,
    Or,
    EqCmp,
    NEqCmp,
    LEqCmp,
    GEqCmp,
    LCmp,
    GCmp,
    OpAdd,
    OpSub,
    OpMult,
    OpDiv,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub ty: TokenType,
    pub lineno: u32,
    pub col: u32,
    pub src: String,
}

impl Token {
    pub fn null() -> Self {
        Self {
            ty: TokenType::LineComment,
            lineno: 0,
            col: 0,
            src: "".to_owned(),
        }
    }
}

pub fn lex(buf: &dyn AsRef<[u8]>) -> Vec<Token> {
    let options = vec![
        (TokenType::LineComment, Regex::new(r"//.*").unwrap()),
        (TokenType::Return, Regex::new(r"return").unwrap()),
        (TokenType::Fn, Regex::new(r"fn").unwrap()),
        (TokenType::If, Regex::new(r"if").unwrap()),
        (TokenType::Else, Regex::new(r"else").unwrap()),
        (TokenType::Let, Regex::new(r"let").unwrap()),
        (
            TokenType::Identifier,
            Regex::new(r"[a-zA-Z][a-zA-Z0-9]*").unwrap(),
        ),
        (TokenType::NumberLit, Regex::new(r"[0-9]+").unwrap()),
        (TokenType::LParen, Regex::new(r"\(").unwrap()),
        (TokenType::RParen, Regex::new(r"\)").unwrap()),
        (TokenType::LBrace, Regex::new(r"\{").unwrap()),
        (TokenType::RBrace, Regex::new(r"\}").unwrap()),
        (TokenType::Semi, Regex::new(r";").unwrap()),
        (TokenType::True, Regex::new(r"true").unwrap()),
        (TokenType::False, Regex::new(r"false").unwrap()),
        (TokenType::Bang, Regex::new(r"!").unwrap()),
        (TokenType::And, Regex::new(r"and").unwrap()),
        (TokenType::Or, Regex::new(r"or").unwrap()),
        (TokenType::EqCmp, Regex::new(r"==").unwrap()),
        (TokenType::EqAssign, Regex::new(r"=").unwrap()),
        (TokenType::NEqCmp, Regex::new(r"!=").unwrap()),
        (TokenType::LEqCmp, Regex::new(r"<=").unwrap()),
        (TokenType::GEqCmp, Regex::new(r">=").unwrap()),
        (TokenType::LCmp, Regex::new(r"<").unwrap()),
        (TokenType::GCmp, Regex::new(r">").unwrap()),
        (TokenType::OpAdd, Regex::new(r"\+").unwrap()),
        (TokenType::OpSub, Regex::new(r"-").unwrap()),
        (TokenType::OpMult, Regex::new(r"\*").unwrap()),
        (TokenType::OpDiv, Regex::new(r"\/").unwrap()),
    ];

    let mut src = Input::from(&buf).anchored(Anchored::Yes);

    let mut result = Vec::<Token>::new();

    let mut next = 0;
    let mut lineno = 1;
    let mut last_newline_idx = 0;
    'next_byte: while next < buf.as_ref().len() {
        src.set_start(next);

        match src.haystack()[next] {
            b'\n' => {
                last_newline_idx = next;
                lineno += 1;
                next += 1;
                continue;
            }
            b' ' | b'\t' => {
                next += 1;
                continue;
            }
            _ => {}
        }

        for (ty, pattern) in &options {
            let Some(m) = pattern.find(src.clone()) else {
                continue;
            };

            result.push(Token {
                ty: *ty,
                lineno,
                col: (next - last_newline_idx) as u32,
                src: String::from_utf8(buf.as_ref()[m.start()..m.end()].to_vec()).unwrap(),
            });
            next += m.len();
            continue 'next_byte;
        }

        dbg!(
            lineno,
            next,
            String::from_utf8(buf.as_ref()[next..min(next + 10, buf.as_ref().len())].to_vec())
                .unwrap()
        );
        panic!("Uninterpreted byte.")
    }

    result
}

#[cfg(test)]
mod tests {
    use crate::parsing::lexing::TokenType;

    use super::lex;

    #[test]
    fn test_sample_1() {
        let tokens = lex(&"
let x = 7; // Comment

fn cat() {
    return 1 + 9
} if else
");

        assert_eq!(tokens[0].ty, TokenType::Let);
        assert_eq!(tokens[0].lineno, 2);
        assert_eq!(tokens[0].src, "let");
        assert_eq!(tokens[1].ty, TokenType::Identifier);
        assert_eq!(tokens[1].lineno, 2);
        assert_eq!(tokens[1].src, "x");
        assert_eq!(tokens[2].ty, TokenType::EqAssign);
        assert_eq!(tokens[2].lineno, 2);
        assert_eq!(tokens[2].src, "=");
        assert_eq!(tokens[3].ty, TokenType::NumberLit);
        assert_eq!(tokens[3].lineno, 2);
        assert_eq!(tokens[3].src, "7");
        assert_eq!(tokens[4].ty, TokenType::Semi);
        assert_eq!(tokens[4].lineno, 2);
        assert_eq!(tokens[4].src, ";");
        assert_eq!(tokens[5].src, "// Comment");
        assert_eq!(tokens[5].ty, TokenType::LineComment);
        assert_eq!(tokens[6].src, "fn");
        assert_eq!(tokens[6].ty, TokenType::Fn);
        assert_eq!(tokens[7].ty, TokenType::Identifier);
        assert_eq!(tokens[8].ty, TokenType::LParen);
        assert_eq!(tokens[9].ty, TokenType::RParen);
        assert_eq!(tokens[10].ty, TokenType::LBrace);
        assert_eq!(tokens[11].ty, TokenType::Return);
        assert_eq!(tokens[12].ty, TokenType::NumberLit);
        assert_eq!(tokens[13].ty, TokenType::OpAdd);
        assert_eq!(tokens[14].ty, TokenType::NumberLit);
        assert_eq!(tokens[15].ty, TokenType::RBrace);
        assert_eq!(tokens[16].ty, TokenType::If);
        assert_eq!(tokens[17].ty, TokenType::Else);
    }
}
