use std::mem::discriminant;

use crate::parser::ParseErr;
use crate::span::Span;
use crate::token::Token;
use crate::token::TokenKind;

pub fn ident(t: Token) -> Result<(String, Span), ParseErr> {
    match t.kind {
        TokenKind::Identifier(ident) => Ok((ident, t.span)),
        _ => Err(ParseErr::UnexpectedToken {
            token: t,
            expected: "Identifier".to_string(),
        }),
    }
}

pub fn assign(t: Token) -> Result<(), ParseErr> {
    match t.kind {
        TokenKind::Assign => Ok(()),
        _ => Err(ParseErr::UnexpectedToken {
            token: t,
            expected: "Assignment (=)".to_string(),
        }),
    }
}

pub fn open_scope(t: Token) -> Result<(), ParseErr> {
    match t.kind {
        TokenKind::OpenCurl => Ok(()),
        _ => Err(ParseErr::UnexpectedToken {
            token: t,
            expected: "{".to_string(),
        }),
    }
}

pub fn type_decl_end(t: Token) -> Result<Token, ParseErr> {
    match t.kind {
        TokenKind::CloseCurl => Ok(t),
        _ => panic!(),
    }
}

/// any of { ( [
pub fn type_classification(t: Token) -> Result<Token, ParseErr> {
    match t.kind {
        TokenKind::OpenCurl | TokenKind::OpenBracket | TokenKind::OpenParen => Ok(t),
        _ => Err(ParseErr::UnexpectedToken {
            token: t,
            expected: "{, [ or (".to_string(),
        }),
    }
}

pub fn any(t: Token) -> Result<Token, ParseErr> {
    Ok(t)
}

pub fn opposit_of<'a>(source: &'a Token) -> impl Fn(Token) -> Result<Token, ParseErr> + 'a {
    |tk| match (&source.kind, &tk.kind) {
        (TokenKind::OpenCurl, TokenKind::CloseCurl) => Ok(tk),
        (TokenKind::OpenCurl, _) => Err(ParseErr::unexpected_token(tk, "}")),
        (TokenKind::OpenParen, TokenKind::CloseParen) => Ok(tk),
        (TokenKind::OpenParen, _) => Err(ParseErr::unexpected_token(tk, ")")),
        (TokenKind::OpenBracket, TokenKind::CloseBracket) => Ok(tk),
        (TokenKind::OpenBracket, _) => Err(ParseErr::unexpected_token(tk, "]")),
        _ => unreachable!(),
    }
}

pub fn any_of<'a>(source: &'a [TokenKind]) -> impl Fn(Token) -> Result<Token, ParseErr> + 'a {
    move |target| {
        for t in source {
            if discriminant(&target.kind) == discriminant(t) {
                return Ok(target);
            }
        }
        let buf = String::with_capacity(10);
        let str = source.iter().fold(buf, |mut acc, t| {
            acc.push_str(t.to_string().as_str());
            acc.push_str(", ");
            acc
        });

        Err(ParseErr::unexpected_token(target, str))
    }
}

pub fn sequence_of<'a>(
    expected: &'a [TokenKind],
) -> impl Fn(Token) -> Result<Token, ParseErr> + 'a {
    |_| todo!()
}

pub fn exact_kind<'a>(source: &'a TokenKind) -> impl Fn(Token) -> Result<Token, ParseErr> + 'a {
    |token| {
        if discriminant(&token.kind) == discriminant(source) {
            return Ok(token);
        }
        Err(ParseErr::unexpected_token(token, source.to_string()))
    }
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

pub fn function_call(_t: &Token) -> bool {
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
