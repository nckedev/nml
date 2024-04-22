enum TokenType {
    //litterals
    String(String),
    Char(u8),
    IntNumber(i64),
    FloatNumber(f64),
    Identifier,
    Litteral,
    Attribute(String),

    //keywords
    Let,
    Mut,
    Interface,
    Type,

    If,
    Else,
    End,
    Do,
    For,

    //one char
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenCurl,
    CloseCurl,

    //one or two char
    Eq,
    NotEq,
    Gt,
    GtEq,
    Lt,
    LtEq,
    Not,

    //binary operators

    //unary operators

    //msc
    Trivia(TokenTrivia),
    None,
}

pub enum TokenTrivia {
    Tab,
    Space,
    EOL,
    EOF,
    Newline,
}

pub enum TokenKeyword {
    Let,
    If,
    Do,
    Else,
    End,
}

pub struct Token {
    token_type: TokenType,
    row: u32,
    start: u32,
    end: u32,
}

impl Token {
    fn new(token_type: TokenType, row: u32, start: u32, end: u32) -> Self {
        Token {
            token_type,
            row,
            start,
            end,
        }
    }
    fn empty() -> Self {
        Self::new(TokenType::None, 0, 0, 0)
    }
}
