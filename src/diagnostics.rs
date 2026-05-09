use std::fmt::Display;
use std::ops::Deref;
use std::slice::Iter;

use crate::parser::ParseErr;
use crate::span::Span;
use crate::token::Token;
use crate::token::TokenKind;

pub struct DiagCode;
impl DiagCode {
    const UNKNOWN: u32 = 0;
    const UNEXPECTED_TOKEN: u32 = 100;
}

#[derive(Debug)]
pub struct DiagEntry {
    id: u32,
    severity: DiagSeverity,
    span: Span,
    message: String,
}

impl DiagEntry {
    fn new(id: u32, severity: DiagSeverity, span: Span, message: String) -> Self {
        Self {
            id,
            severity,
            span,
            message,
        }
    }
    pub fn message_only(message: String) -> Self {
        Self {
            id: 0,
            severity: DiagSeverity::Error,
            span: Span::default(),
            message: message.to_string(),
        }
    }

    pub fn empty(message: String) -> Self {
        Self {
            id: 0,
            severity: DiagSeverity::Error,
            span: Span::default(),
            message,
        }
    }

    pub fn expected(found: &Token, expected: String) {}
}

impl Display for DiagEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "---")?;
        writeln!(f, "{}", self.message)?;
        writeln!(f, "---")?;
        Ok(())
    }
}

impl TryFrom<ParseErr> for DiagEntry {
    type Error = ();

    fn try_from(value: ParseErr) -> Result<Self, Self::Error> {
        match value {
            ParseErr::UnexpectedToken { token, expected } => {
                let expected_str = match expected.len() {
                    0 => Err(())?,
                    1 => expected.first().unwrap().to_string(),
                    _ => {
                        let buffer = String::with_capacity(10);
                        expected.iter().fold(buffer, |mut b, e| {
                            b.push_str(&format!("{e},"));
                            b
                        })
                    }
                };
                Ok(DiagEntry {
                    id: DiagCode::UNEXPECTED_TOKEN,
                    severity: DiagSeverity::Error,
                    span: token.span,
                    message: format!("Expected {}, found {}", expected_str, token),
                })
            }
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
pub enum DiagSeverity {
    Error,
    Warn,
    Info,
}

struct DiagEntryBuilder {}

pub struct Diagnostics {
    list: Vec<DiagEntry>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self { list: vec![] }
    }

    pub fn reset(&mut self) {
        self.list.clear()
    }

    pub fn push(&mut self, entry: DiagEntry) {
        self.list.push(entry)
    }
    pub fn push_message(&mut self, severity: DiagSeverity, message: &str) {
        let m = DiagEntry::new(
            0,
            severity,
            Span::from(((0, 0).into(), (0, 0).into())),
            String::from(message),
        );
        self.list.push(m);
    }

    pub fn push_expected_token_missmatch(
        &mut self,
        actual: &TokenKind,
        expected: String,
        span: &Span,
    ) {
        let entry = DiagEntry::message_only(format!(
            "@ expected {}, found {}",
            expected.as_str(),
            actual
        ));

        self.list.push(entry);
    }

    pub fn iter(&'_ self) -> Iter<'_, DiagEntry> {
        self.list.iter()
    }
}

impl IntoIterator for Diagnostics {
    type Item = DiagEntry;

    type IntoIter = <Vec<DiagEntry> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.list.into_iter()
    }
}

impl Deref for Diagnostics {
    type Target = [DiagEntry];

    fn deref(&self) -> &Self::Target {
        &self.list[..]
    }
}
