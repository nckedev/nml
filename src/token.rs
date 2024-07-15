#![allow(dead_code)]

use crate::pos::Pos;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    //litterals
    String(String),
    Char(u8),
    IntNumber(i64),
    FloatNumber(f64),
    Nan(String),
    Identifier(String),
    Litteral,
    Attribute(String),
    Discard,

    //keywords
    Let,
    Mut,
    Interface,
    Variant,
    Trait,
    Struct,
    Type,
    Ref,
    SelfRef,
    Yied,
    Const,

    If,
    Else,
    End,
    Do,
    For,
    In,

    //one char
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenCurl,
    CloseCurl,

    //one or two char
    Assign,
    Eq,
    NotEq,
    Gt,
    GtEq,
    Lt,
    LtEq,

    Lambda,
    Range(bool),
    MethodAccessor,

    //binary operators
    Plus,
    Minus,
    Mul,
    Div,
    Mod,

    //unary operators
    Not,
    Neg,
    Inc,
    Dec,

    //logical operators
    And,
    Or,

    //msc
    Trivia(TokenTrivia),
    None,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenTrivia {
    Tab,
    Space,
    EOL,
    EOF,
}

pub enum Keyword {
    IF,
    LET,
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub pos: Pos,
}

impl Token {
    pub fn new(token_type: TokenType, pos: Pos) -> Self {
        Token { token_type, pos }
    }
    fn empty() -> Self {
        Self::new(TokenType::None, Pos::zero())
    }
}
