#![allow(dead_code)]

use crate::source_char::SourceIndex;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    //litterals
    String(String),
    Char(u8),
    Number(NumberToken),
    // IntLit(i64),
    // UIntLit(String, Option<String>),
    // FloatLit(f64),
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
    InclusiveRange,
    ExclusiveRange,
    OpenStartRange,
    OpenEndRange,
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
    Error(TokenError),
    Empty,
}

impl TokenKind {
    /// Returns `true` if the token kind is [`Error`].
    ///
    /// [`Error`]: TokenKind::Error
    #[must_use]
    pub fn is_error(&self) -> bool {
        matches!(self, Self::Error(..))
    }

    /// Returns `true` if the token kind is [`Trivia`].
    ///
    /// [`Trivia`]: TokenKind::Trivia
    #[must_use]
    pub fn is_trivia(&self) -> bool {
        matches!(self, Self::Trivia(..))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NumberToken {
    pub value: String,
    pub prefix: Option<String>,
    pub suffix: Option<String>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenError {
    Uknown,
    Unexpected,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenTrivia {
    Tab,
    Space,
    EOL,
    EOF,
}

/// TokenSpan
#[derive(Debug)]
pub struct TokenSpan {
    pub start: SourceIndex,
    pub end: SourceIndex,
}

impl TokenSpan {
    pub fn empty() -> Self {
        Self {
            start: SourceIndex::emtpy(),
            end: SourceIndex::emtpy(),
        }
    }
}

/// Token
#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: TokenSpan,
}

impl Token {
    pub fn new(kind: TokenKind, span: TokenSpan) -> Self {
        Token { kind, span }
    }
    fn empty() -> Self {
        Self::new(TokenKind::Empty, TokenSpan::empty())
    }
}
