#![cfg(test)]

use crate::{
    ast::Node,
    token::{self, Token, TokenKind},
};

pub fn assert_snapshot(v: impl SnapshotStr) {
    insta::assert_snapshot!(v.snapshot());
}

pub trait SnapshotStr {
    fn collect(&self, depth: usize, buf: &mut String);

    fn snapshot(&self) -> String {
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
    fn collect(&self, _: usize, buf: &mut String) {
        let str = match &self.kind {
            TokenKind::String(str) => &format!("String \"{}\"", str),
            TokenKind::Char(c) => &format!("Char '{}'", c),
            TokenKind::Number(number_token) => {
                let prefix = match number_token.prefix {
                    token::NumberTokenPrefix::None => "",
                    token::NumberTokenPrefix::Bin => "Bin",
                    token::NumberTokenPrefix::Hex => "Hex",
                    token::NumberTokenPrefix::Oct => "Oct",
                    token::NumberTokenPrefix::Dot => ".",
                    token::NumberTokenPrefix::Invalid(x) => &format!("Invalid({x})"),
                };
                let suffix = match number_token.suffix {
                    token::NumberTokenSuffix::None => "",
                    token::NumberTokenSuffix::Float => "float",
                    token::NumberTokenSuffix::Uint => "Uint",
                    token::NumberTokenSuffix::Int => "Int",
                    token::NumberTokenSuffix::Dot => ".",
                    token::NumberTokenSuffix::Sientific => "Sientific",
                };
                &format!("Number: {}{}{}", prefix, number_token.value, suffix)
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
            TokenKind::PlusAssign => "PlusAssign",
            TokenKind::Minus => "Minus",
            TokenKind::MinusAssign => "MinusAssign",
            TokenKind::Mul => "Mul",
            TokenKind::MulAssign => "MulAssign",
            TokenKind::Div => "Div",
            TokenKind::DivAssign => "DivAssign",
            TokenKind::Mod => "Mod",
            TokenKind::ModAssign => "ModAssign",
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
                };
                &str.clone()
            }
            TokenKind::Error(token_error) => match token_error {
                token::TokenError::Uknown => "Error uknown",
                token::TokenError::Unexpected(c) => &format!("Error Unexpected: {}", c),
            },
            TokenKind::Empty => "Empty",
            TokenKind::Eof => "EOF",
            TokenKind::Eol => "EOL",
        };

        // let asdf     20:20 - 20:21
        buf.push_str(&format!(
            "{:>4}:{:<2} - {:>4}:{:<2} {}\n",
            self.span.start.row, self.span.start.col, self.span.end.row, self.span.end.col, str
        ));
    }
}
