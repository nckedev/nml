#![cfg(test)]
use std::fmt::Display;

use crate::{
    ast::Node,
    token::{self, Token, TokenKind},
};

pub fn assert_snapshot(v: impl SnapshotStr) {
    insta::assert_snapshot!(v.print());
}

pub trait SnapshotStr {
    fn collect(&self, depth: usize, buf: &mut String);

    fn print(&self) -> String {
        let mut buf = String::with_capacity(100);
        self.collect(0, &mut buf);
        buf
    }
}

impl<P> SnapshotStr for Vec<P>
where
    P: SnapshotStr,
{
    fn collect(&self, depth: usize, buf: &mut String) {
        for n in self {
            n.collect(depth, buf);
        }
    }
}

impl SnapshotStr for Node {
    fn collect(&self, depth: usize, buf: &mut String) {
        buf.push_str(&"\t".repeat(depth));
        match self {
            Node::VariableAccess => buf.push_str("VariableAccess"),
            Node::FunctionCall => buf.push_str("FunctionCall"),
            Node::MethodCall => buf.push_str("MethodCall"),
            Node::TypeDecl {
                type_id,
                ident,
                body,
            } => {
                buf.push_str("TypeDecl\n");
                body.collect(depth + 1, buf)
            }
            Node::RecordFieldDecl { name_ident } => buf.push_str("RecordFieldDecl\n"),
            Node::BlockStmt => buf.push_str("BlockStmt\n"),
            Node::UseStmt {} => buf.push_str("UseStmt\n"),
            Node::ModuleDeclr { ident, body } => buf.push_str("ModuleDeclr\n"),
            Node::LetStmt { span, ident, expr } => buf.push_str("LetStmt\n"),
            Node::Ident { ident } => buf.push_str("Ident\n"),
            Node::TypeIdent { ident } => buf.push_str("TypeIdent\n"),
            Node::IfExpr => buf.push_str("IfExpr\n"),
            Node::MatchExpr => buf.push_str("MatchExpr\n"),
            Node::ConstExpr { expr } => buf.push_str("ConstExpr\n"),
            Node::BinaryExpr {
                left,
                operator,
                right,
            } => {
                buf.push_str("BinaryExpr\n");
                left.collect(depth + 1, buf);
                buf.push_str(&format!("\t{}", operator));
                right.collect(depth + 1, buf);
            }
            Node::BooleanExpr {
                left,
                operator,
                right,
            } => todo!(),
            Node::Block { stmts, span } => buf.push_str("Block\n"),
            Node::UnaryExpr => buf.push_str("UnaryExpr\n"),
            Node::EOF => buf.push_str("EOF\n"),
            Node::Invalid => buf.push_str("Invalid\n"),
            Node::Empty => buf.push_str("Empty\n"),
        };
    }
}

impl SnapshotStr for Token {
    fn collect(&self, depth: usize, buf: &mut String) {
        let str = match &self.kind {
            TokenKind::String(str) => &format!("String \"{}\"", str),
            TokenKind::Char(c) => &format!("Char '{}'", c),
            TokenKind::Number(number_token) => {
                let mut str = format!("Number: {}", number_token.value);
                if let Some(suffix) = &number_token.suffix {
                    str.push_str(suffix);
                }
                &str.clone()
            }
            TokenKind::Identifier(ident) => &format!("Identifier: {}", ident),
            TokenKind::Litteral => "Litteral",
            TokenKind::Discard => "Discard",
            TokenKind::Let => "Let",
            TokenKind::Trait => "Trait",
            TokenKind::Attribute => "Attribute",
            TokenKind::Type => "Type",
            TokenKind::Macro => "Macro",
            TokenKind::Todo => "Todo",
            TokenKind::Panic => "Panic",
            TokenKind::Module => "Module",
            TokenKind::Void => "Void",
            TokenKind::Pub => "Pub",
            TokenKind::Opaque => "Opaque",
            TokenKind::DBG => "DBG",
            TokenKind::If => "If",
            TokenKind::Else => "Else",
            TokenKind::For => "For",
            TokenKind::In => "In",
            TokenKind::Try => "Try",
            TokenKind::Guard => "Guard",
            TokenKind::Arrow => "Arrow",
            TokenKind::FatArrow => "FatArrow",
            TokenKind::OpenParen => "OpenParen",
            TokenKind::CloseParen => "CloseParen",
            TokenKind::OpenBracket => "OpenBracket",
            TokenKind::CloseBracket => "CloseBracket",
            TokenKind::OpenCurl => "OpenCurl",
            TokenKind::CloseCurl => "CloseCurl",
            TokenKind::Separator => "Separator",
            TokenKind::Assign => "Assign",
            TokenKind::Eq => "Eq",
            TokenKind::NotEq => "NotEq",
            TokenKind::Gt => "Gt",
            TokenKind::GtEq => "GtEq",
            TokenKind::Lt => "Lt",
            TokenKind::LtEq => "LtEq",
            TokenKind::InclusiveRange => "InclusiveRange",
            TokenKind::ExclusiveRange => "ExclusiveRange",
            TokenKind::OpenStartRange => "OpenStartRange",
            TokenKind::OpenEndRange => "OpenEndRange",
            TokenKind::MethodAccessor => "MethodAccessor",
            TokenKind::Plus => "Plus",
            TokenKind::Minus => "Minus",
            TokenKind::Mul => "Mul",
            TokenKind::Div => "Div",
            TokenKind::Mod => "Mod",
            TokenKind::Not => "Not",
            TokenKind::Neg => "Neg",
            TokenKind::Inc => "Inc",
            TokenKind::Dec => "Dec",
            TokenKind::And => "And",
            TokenKind::Or => "Or",
            TokenKind::AtMarker => "AtMarker",
            TokenKind::Trivia(token_trivia) => {
                let mut str = String::from("Trivia ");
                match token_trivia {
                    token::TokenTrivia::Tab => str.push_str("Tab"),
                    token::TokenTrivia::Space => str.push_str("Space"),
                    token::TokenTrivia::EOL => str.push_str("End of Line"),
                    token::TokenTrivia::EOF => str.push_str("End of File"),
                };
                &str.clone()
            }
            TokenKind::Error(token_error) => "Error",
            TokenKind::Empty => "Empty",
        };

        // let asdf     20:20 - 20:21
        buf.push_str(&format!(
            "{:>4}:{:<2} - {:>4}:{:<2} {}\n",
            self.span.start.row, self.span.start.col, self.span.end.row, self.span.end.col, str
        ));
    }
}

// impl Display for Token {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let str = match &self.kind {
//             token::TokenKind::String(str) => &format!("String \"{}\"", str),
//             token::TokenKind::Char(c) => &format!("Char '{}'", c),
//             token::TokenKind::Number(number_token) => {
//                 let mut str = String::from(&number_token.value);
//                 if let Some(suffix) = &number_token.suffix {
//                     str.push_str(suffix);
//                 }
//                 &str.clone()
//             }
//             token::TokenKind::Identifier(_) => "Identifier",
//             token::TokenKind::Litteral => "Litteral",
//             token::TokenKind::Discard => "Discard",
//             token::TokenKind::Let => "Let",
//             token::TokenKind::Trait => "Trait",
//             token::TokenKind::Attribute => "Attribute",
//             token::TokenKind::Type => "Type",
//             token::TokenKind::Macro => "Macro",
//             token::TokenKind::Todo => "Todo",
//             token::TokenKind::Panic => "Panic",
//             token::TokenKind::Module => "Module",
//             token::TokenKind::Void => "Void",
//             token::TokenKind::Pub => "Pub",
//             token::TokenKind::Opaque => "Opaque",
//             token::TokenKind::DBG => "DBG",
//             token::TokenKind::If => "If",
//             token::TokenKind::Else => "Else",
//             token::TokenKind::For => "For",
//             token::TokenKind::In => "In",
//             token::TokenKind::Try => "Try",
//             token::TokenKind::Guard => "Guard",
//             token::TokenKind::Arrow => "Arrow",
//             token::TokenKind::FatArrow => "FatArrow",
//             token::TokenKind::OpenParen => "OpenParen",
//             token::TokenKind::CloseParen => "CloseParen",
//             token::TokenKind::OpenBracket => "OpenBracket",
//             token::TokenKind::CloseBracket => "CloseBracket",
//             token::TokenKind::OpenCurl => "OpenCurl",
//             token::TokenKind::CloseCurl => "CloseCurl",
//             token::TokenKind::Separator => "Separator",
//             token::TokenKind::Assign => "Assign",
//             token::TokenKind::Eq => "Eq",
//             token::TokenKind::NotEq => "NotEq",
//             token::TokenKind::Gt => "Gt",
//             token::TokenKind::GtEq => "GtEq",
//             token::TokenKind::Lt => "Lt",
//             token::TokenKind::LtEq => "LtEq",
//             token::TokenKind::InclusiveRange => "InclusiveRange",
//             token::TokenKind::ExclusiveRange => "ExclusiveRange",
//             token::TokenKind::OpenStartRange => "OpenStartRange",
//             token::TokenKind::OpenEndRange => "OpenEndRange",
//             token::TokenKind::MethodAccessor => "MethodAccessor",
//             token::TokenKind::Plus => "Plus",
//             token::TokenKind::Minus => "Minus",
//             token::TokenKind::Mul => "Mul",
//             token::TokenKind::Div => "Div",
//             token::TokenKind::Mod => "Mod",
//             token::TokenKind::Not => "Not",
//             token::TokenKind::Neg => "Neg",
//             token::TokenKind::Inc => "Inc",
//             token::TokenKind::Dec => "Dec",
//             token::TokenKind::And => "And",
//             token::TokenKind::Or => "Or",
//             token::TokenKind::AtMarker => "AtMarker",
//             token::TokenKind::Trivia(token_trivia) => "Trivia",
//             token::TokenKind::Error(token_error) => "Error",
//             token::TokenKind::Empty => "Empty",
//         };
//
//         // let asdf     20:20 - 20:21
//         write!(
//             f,
//             "{}",
//             format!(
//                 "{:>4}:{:<2} - {:>4}:{:<2} {}\n",
//                 self.span.start.row, self.span.start.col, self.span.end.row, self.span.end.col, str
//             )
//         )?;
//         Ok(())
//     }
// }
