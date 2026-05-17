#![allow(dead_code)]

use crate::span::Span;

use std::fmt::Display;

#[derive(Debug, PartialEq, Clone, Default)]
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
    /// `_`
    Discard,

    //keywords
    Let,
    // Mut,
    // Struct,
    // Interface,
    // Enum,
    Trait,
    // Variant, // or enum??
    Attribute,
    Type,
    Macro,
    Todo, //should be a macro
    Panic,
    Module,
    Void,
    Pub,
    Opaque,
    DBG,

    If,
    Else,
    // End,
    // Do,
    For,
    In,

    Try,
    Guard,

    //arrows
    /// ->
    Arrow,
    /// =>
    FatArrow,

    //one char
    /// (
    OpenParen,
    /// )
    CloseParen,
    /// [
    OpenBracket,
    /// ]
    CloseBracket,
    /// {
    OpenCurl,
    /// }
    CloseCurl,
    /// ,
    Separator,

    //one or two char
    /// =
    Assign,
    /// ==
    Eq,
    /// !=
    NotEq,
    /// `>`
    Gt,
    /// `>=`
    GtEq,
    /// `<`
    Lt,
    /// `<=`
    LtEq,

    /// ..=
    InclusiveRange,
    /// ..
    ExclusiveRange,
    OpenStartRange,
    OpenEndRange,
    MethodAccessor,

    //binary operators
    /// +
    Plus,
    /// +=
    PlusAssign,
    /// -
    Minus,
    /// -=
    MinusAssign,
    /// *
    Mul,
    /// *=
    MulAssign,
    /// /
    Div,
    /// /=
    DivAssign,
    /// %
    Mod,
    /// %=
    ModAssign,

    //unary operators
    Not,
    Neg,
    Inc,
    Dec,

    //logical operators
    And,
    Or,

    /// @
    AtMarker,

    //msc
    Trivia(TokenTrivia),
    Error(TokenError),
    #[default]
    Empty,
    Eof,
    Eol,
    Invalid,
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

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            TokenKind::Plus => write!(f, "+"),
            TokenKind::String(_) => write!(f, "String"),
            TokenKind::Char(_) => write!(f, "Char"),
            TokenKind::Number(_) => write!(f, "Number"),
            TokenKind::Identifier(_) => write!(f, "Identifier"),
            TokenKind::Litteral => write!(f, "Litteral"),
            TokenKind::Discard => write!(f, "Discard"),
            TokenKind::Let => write!(f, "Let"),
            TokenKind::Attribute => write!(f, "Attr"),
            TokenKind::Type => write!(f, "Type"),
            TokenKind::Macro => write!(f, "Macro"),
            TokenKind::Todo => write!(f, "Todo"),
            TokenKind::Panic => write!(f, "Panic"),
            TokenKind::Module => write!(f, "Module"),
            TokenKind::Void => write!(f, "Void"),
            TokenKind::DBG => write!(f, "DBG"),
            TokenKind::If => write!(f, "If"),
            TokenKind::Else => write!(f, "Else"),
            TokenKind::For => write!(f, "For"),
            TokenKind::In => write!(f, "In"),
            TokenKind::Try => write!(f, "Try"),
            TokenKind::Guard => write!(f, "Guard"),
            TokenKind::Arrow => write!(f, "->"),
            TokenKind::FatArrow => write!(f, "=>"),
            TokenKind::OpenParen => write!(f, "("),
            TokenKind::CloseParen => write!(f, ")"),
            TokenKind::OpenBracket => write!(f, "["),
            TokenKind::CloseBracket => write!(f, "]"),
            TokenKind::OpenCurl => write!(f, "{{"),
            TokenKind::CloseCurl => write!(f, "}}"),
            TokenKind::Separator => write!(f, "Separator"),
            TokenKind::Assign => write!(f, "Assign"),
            TokenKind::Eq => write!(f, "=="),
            TokenKind::NotEq => write!(f, "!="),
            TokenKind::Gt => write!(f, ">"),
            TokenKind::GtEq => write!(f, ">="),
            TokenKind::Lt => write!(f, "<"),
            TokenKind::LtEq => write!(f, "<="),
            TokenKind::InclusiveRange => write!(f, "..="),
            TokenKind::ExclusiveRange => write!(f, ".."),
            TokenKind::OpenStartRange => write!(f, ".."),
            TokenKind::OpenEndRange => write!(f, ".."),
            TokenKind::MethodAccessor => write!(f, "."),
            TokenKind::Div => write!(f, "/"),
            TokenKind::Not => write!(f, "!"),
            TokenKind::Neg => write!(f, "-"),
            TokenKind::Inc => write!(f, ""),
            TokenKind::Dec => write!(f, ""),
            TokenKind::And => write!(f, "and"),
            TokenKind::Or => write!(f, "or"),
            TokenKind::AtMarker => write!(f, "@"),
            TokenKind::Trivia(_) => write!(f, "Trivia"),
            TokenKind::Error(_) => write!(f, "Err"),
            TokenKind::Empty => write!(f, "Empty"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Mul => write!(f, "*"),
            TokenKind::Mod => write!(f, "%"),
            TokenKind::Pub => write!(f, "pub"),
            TokenKind::Opaque => write!(f, "opaque"),
            TokenKind::Trait => write!(f, "trait"),
            TokenKind::Eof => write!(f, "end of file"),
            TokenKind::Eol => write!(f, "end of line"),
            TokenKind::PlusAssign => write!(f, "plus assign"),
            TokenKind::MinusAssign => write!(f, "minus assign"),
            TokenKind::MulAssign => write!(f, "mul assign"),
            TokenKind::DivAssign => write!(f, "div assign"),
            TokenKind::ModAssign => write!(f, "mod assign"),
            _ => write!(f, "NOT IMPLEMENTED"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct NumberToken {
    pub value: String,
    /// prefixes
    /// 0x - hex 0x23AF
    /// 0b - bin 0b0101_0111
    /// 0o - oct 0o1281_1277
    /// .  - dec .1 == 0.1
    pub prefix: NumberTokenPrefix,
    /// suffixes
    /// f - float
    /// u - unsigned int
    /// e - sientific notation ue .2e-23
    pub suffix: NumberTokenSuffix,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum NumberTokenPrefix {
    None,
    Bin,
    Hex,
    Oct,
    Dot,
    Invalid(char),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum NumberTokenSuffix {
    None,
    Float,
    Uint,
    Int,
    Dot,
    Sientific,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenError {
    Uknown,
    Unexpected(char),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenTrivia {
    Tab,
    Space,
}

/// Token
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Token { kind, span }
    }

    fn empty() -> Self {
        Self::new(TokenKind::Empty, Span::default())
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}
