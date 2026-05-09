use crate::parser::ParseErr;
use crate::span::Span;
use crate::token::Token;
use crate::token::TokenKind;

pub fn ident(t: Token) -> Result<(String, Span), ParseErr> {
    match t.kind {
        TokenKind::Identifier(ident) => Ok((ident, t.span)),
        _ => Err(ParseErr::UnexpectedToken {
            token: t,
            expected: vec![TokenKind::Identifier("Ident".to_string())],
        }),
    }
}

pub fn assign(t: Token) -> Result<(), ParseErr> {
    match t.kind {
        TokenKind::Assign => Ok(()),
        _ => Err(ParseErr::UnexpectedToken {
            token: t,
            expected: vec![TokenKind::Assign],
        }),
    }
}

pub fn open_scope(t: Token) -> Result<(), ParseErr> {
    match t.kind {
        TokenKind::OpenCurl => Ok(()),
        _ => Err(ParseErr::UnexpectedToken {
            token: t,
            expected: vec![TokenKind::OpenCurl],
        }),
    }
}

pub fn type_decl_end(t: Token) -> Result<(), ParseErr> {
    match t.kind {
        TokenKind::CloseCurl => Ok(()),
        _ => panic!(),
    }
}

pub fn type_classification(t: Token) -> Result<Token, ParseErr> {
    match t.kind {
        TokenKind::OpenCurl | TokenKind::OpenBracket | TokenKind::OpenParen => Ok(t),
        _ => Err(ParseErr::UnexpectedToken {
            token: t,
            expected: vec![
                TokenKind::CloseCurl,
                TokenKind::CloseBracket,
                TokenKind::CloseParen,
            ],
        }),
    }
}

pub fn any(t: Token) -> Result<Token, ParseErr> {
    Ok(t)
}

pub fn token_kind(t: Token, pred: &impl Fn(&TokenKind) -> bool) -> Result<Token, ParseErr> {
    if pred(&t.kind) {
        Ok(t)
    } else {
        Err(ParseErr::NotSupported)
    }
}

pub fn is_open_scope(t: &Token) -> bool {
    match t.kind {
        TokenKind::OpenCurl => true,
        _ => false,
    }
}

pub fn is_const_expr(t: &Token) -> bool {
    match t.kind {
        TokenKind::Number(..) | TokenKind::String(..) => true,
        _ => false,
    }
}

pub fn function_call(t: &Token) -> bool {
    todo!()
}

pub fn is_operator(t: &Token) -> bool {
    matches!(
        t.kind,
        TokenKind::Plus | TokenKind::Minus | TokenKind::Mul | TokenKind::Div
    )
}
pub fn operator_addative(t: &Token) -> bool {
    matches!(t.kind, TokenKind::Plus | TokenKind::Minus)
}

pub fn operator_multiplicative(t: &Token) -> bool {
    matches!(t.kind, TokenKind::Mul | TokenKind::Div | TokenKind::Mod)
}

pub fn identifier(t: &Token) -> bool {
    matches!(t.kind, TokenKind::Identifier(_))
}

#[cfg(test)]
mod tests {
    use crate::{parser::Parser, scope::IdGenerator, span::Span, std::assert};

    use super::*;

    #[test]
    fn expect_identifier() {
        let a = Token::new(TokenKind::Identifier("test".into()), Span::default());

        assert!(identifier(&a));
    }
    #[test]
    fn expect_not_identifier() {
        let a = Token::new(TokenKind::Plus, Span::default());

        assert!(!identifier(&a));
    }
}
