use std::fmt::Display;

use crate::{
    ast::{Ast, Node, Untyped},
    diagnostics::{DiagEntry, DiagSeverity::Error, Diagnostics},
    expected_token,
    identifier::Identifier,
    log::Log,
    scope::{IdGenerator, ScopeId, TypeId},
    source_char::SourceIndex,
    span::Span,
    table::TypeTable,
};
use crate::{
    stream::Stream,
    token::{Token, TokenKind, TokenTrivia},
};

pub struct Parser<'a> {
    stream: Stream<Token>,
    diagnostics: &'a mut Diagnostics,
    id_generator: &'a mut IdGenerator,
    type_table: TypeTable,
    // use_table: Vec<String>,
    // scope_table: Vec<String>,
}

#[derive(Debug)]
pub enum ParseErr {
    UnexpectedEndOfFile,
    UnexpectedToken {
        token: Token,
        expected: Vec<TokenKind>,
    },
    NotSupported,
    NotYetImplemented,
}

pub trait NoMoreTokens {
    fn no_more_tokens() -> Self;
}

impl NoMoreTokens for ParseErr {
    fn no_more_tokens() -> Self {
        Self::UnexpectedEndOfFile
    }
}

impl<'a> Parser<'a> {
    pub fn new(
        tokens: Vec<Token>,
        id_generator: &'a mut IdGenerator,
        diagnostics: &'a mut Diagnostics,
    ) -> Self {
        //strip whitespace
        let t: Vec<Token> = tokens
            .into_iter()
            .filter(|x| {
                x.kind != TokenKind::Trivia(TokenTrivia::Space)
                    && x.kind != TokenKind::Trivia(TokenTrivia::Tab)
            })
            .collect::<Vec<Token>>();
        Parser {
            stream: Stream::from(t),
            diagnostics,
            id_generator,
            type_table: TypeTable::new(),
        }
    }

    // fn print(&self) {
    //     for x in &self.stream {
    //         debug::print(&x);
    //     }
    // }

    pub fn get_diagnostics(&self) -> &Diagnostics {
        &self.diagnostics
    }

    pub fn parse(&mut self) -> Result<Ast<Untyped>, ParseErr> {
        Log::info("Parsing");
        let root = self.id_generator.next_scope();

        //self.print();

        let b = self.parse_stmt(root);
        let mut ast = Ast::new();
        // debug::print(&b);
        ast.add(b?);

        Ok(ast)
    }

    //parse order -> addative -> multiplicative -> const
    //

    fn parse_addative_expr(&mut self) -> Result<Node, ParseErr> {
        let mut left = self.parse_multiplicative_expr()?;

        let t = self.stream.take_expecting(|t| {
            expected_token::token_kind(t, &|kind| matches!(kind, TokenKind::Number(..)))
        });

        while let Some(token) = self.stream.take_if_fn(expected_token::operator_addative) {
            let op = token.kind;
            let right = self.parse_multiplicative_expr()?;
            left = Node::BinaryExpr {
                left: Box::new(left),
                operator: op.into(),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_multiplicative_expr(&mut self) -> Result<Node, ParseErr> {
        let mut left = self.parse_const_expr()?;

        while let Some(token) = self
            .stream
            .take_if_fn(expected_token::operator_multiplicative)
        {
            let op = token.kind;
            let right = self.parse_const_expr()?;
            left = Node::BinaryExpr {
                left: Box::new(left),
                operator: op.into(),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn parse_const_expr(&mut self) -> Result<Node, ParseErr> {
        let Some(token) = self.stream.take() else {
            return Err(ParseErr::UnexpectedEndOfFile);
        };

        let res = match token.kind {
            TokenKind::Number(x) => Node::ConstExpr { expr: x.value },
            TokenKind::Identifier(ident) => {
                // TODO: variable lookup
                todo!()
            }
            // TODO: function call?
            x => {
                self.diagnostics
                    .push_expected_token_missmatch(&x, "number".into(), &token.span);
                println!("invalid 2 {:?}", x);
                Node::Invalid
            }
        };

        Ok(res)
    }

    //statements
    //let binding
    //fn declr
    //type declr
    //if statement
    //for loop
    //return
    fn parse_stmt(&mut self, scope: ScopeId) -> Result<Node, ParseErr> {
        // let Some(stmt_token) = self.stream.take() else {
        //     return Err(ParseErr::UnexpectedEndOfFile);
        // };

        let stmt = self.stream.peek_expecting(expected_token::any)?;

        // debug::print(&token);

        let res = match stmt.kind {
            TokenKind::Let => self.parse_let_binding(scope)?,
            TokenKind::Module => self.parse_module_decl(scope)?,
            TokenKind::Type => {
                // TODO: type declr, and properties can have attributes
                // type RecordType = {
                //    @attr ident Type,
                //    @attr ident2 Type2,
                // }

                // take identifier
                let (ident, span) = self.stream.take_expecting(expected_token::ident)?;
                if ident.chars().next().is_some_and(char::is_lowercase) {
                    self.diagnostics
                        .push_message(Error, "Type Identifiers need to be Capitalized");
                }

                // take the '='
                let _ = self.stream.take_expecting(expected_token::assign)?;

                // take the 'struct | interface | enum'
                let token = self
                    .stream
                    .take_expecting(expected_token::type_classification)?;

                let type_class_body = match token.kind {
                    TokenKind::OpenCurl => self.parse_struct(scope)?,
                    TokenKind::OpenBracket => self.parse_enum(scope)?,
                    TokenKind::OpenParen => self.parse_tuple(scope)?,
                    // TokenKind::Interface => self.parse_interface(scope)?,
                    _ => unreachable!(),
                };

                //take the  '{'

                Node::TypeDecl {
                    type_id: self.id_generator.next_type(),
                    ident: Identifier::new(ident, span),
                    body: Box::new(type_class_body),
                }
            }
            TokenKind::Trivia(TokenTrivia::EOL) => self.parse_stmt(scope)?,
            TokenKind::Trivia(TokenTrivia::EOF) => Node::EOF,
            x => {
                self.diagnostics
                    .push(DiagEntry::empty("invalid token".to_string()));
                self.parse_stmt(scope)?
            }
        };
        Ok(res)
    }

    fn parse_let_binding(&mut self, scope: ScopeId) -> Result<Node, ParseErr> {
        // take and discard the let keyword
        let _ = self.stream.take();
        let (ident, span) = self.stream.take_expecting(expected_token::ident)?;

        //take the '=' token
        self.stream.take_expecting(expected_token::assign)?;
        let expr = self.parse_addative_expr()?;

        Ok(Node::LetStmt {
            span: Span::from((span.start, SourceIndex::from((0, 0)))),
            ident: Identifier::new(ident, span),
            expr: Box::new(expr),
        })
    }

    fn parse_module_decl(&mut self, scope: ScopeId) -> Result<Node, ParseErr> {
        let _ = self.stream.take();
        let token = self.stream.take_or(ParseErr::UnexpectedEndOfFile)?;

        let s = self.id_generator.next_scope();
        match token.kind {
            TokenKind::Identifier(ident) => Ok(Node::ModuleDeclr {
                ident,
                body: vec![self.parse_stmt(s)?],
            }),
            _ => {
                self.diagnostics.push_expected_token_missmatch(
                    &token.kind,
                    "identifier".to_string(),
                    &token.span,
                );
                Err(ParseErr::UnexpectedToken {
                    token,
                    expected: vec![TokenKind::Identifier("Ident".to_string())],
                })
            }
        }
    }

    fn parse_type_decl_body(&mut self, scope: ScopeId) -> Result<Node, ParseErr> {
        let token = self
            .stream
            .take_expecting(expected_token::type_classification)?;

        let body = match token.kind {
            TokenKind::OpenCurl => self.parse_struct(scope)?,
            TokenKind::OpenBracket => self.parse_enum(scope)?,
            TokenKind::OpenParen => self.parse_tuple(scope)?,
            // TokenKind::Interface => self.parse_interface(scope)?,
            _ => unreachable!(),
        };
        Ok(Node::Invalid)
    }

    fn parse_struct(&mut self, scope: ScopeId) -> Result<Node, ParseErr> {
        //pasrse the
        //{
        //  a type,
        //  b type
        //}
        //part of a type declr
        let (ident, span) = self.stream.take_expecting(expected_token::ident)?;
        let (type_ident, type_span) = self.stream.take_expecting(expected_token::ident)?;
        let _ = self.stream.take_expecting(expected_token::type_decl_end);

        Ok(Node::RecordFieldDecl {
            name_ident: Identifier { value: ident, span },
        })
    }
    fn parse_interface(&mut self, scope: ScopeId) -> Result<Node, ParseErr> {
        todo!()
    }
    fn parse_enum(&mut self, scope: ScopeId) -> Result<Node, ParseErr> {
        todo!("enums nyi")
    }
    fn parse_tuple(&mut self, scope: ScopeId) -> Result<Node, ParseErr> {
        todo!("tuple nyi")
    }
}

struct MetaData {
    id: String,
    attributes: Option<String>,
    type_info: TypeInfo,
    scope: ScopeId,
}

pub struct TypeInfo {
    pub id: TypeId,
    indentifier: String,
    pub full_identifier: String,
    declaring_scope: ScopeId,
    access_mod: String,
    _type: String,
}

#[derive(Debug)]
pub(crate) enum Operator {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
}

impl From<TokenKind> for Operator {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Plus => Operator::Plus,
            TokenKind::Minus => Operator::Minus,
            TokenKind::Mul => Operator::Mul,
            TokenKind::Div => Operator::Div,
            TokenKind::Mod => Operator::Mod,
            _ => todo!(),
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Operator::Plus => write!(f, "+"),
            Operator::Minus => write!(f, "-"),
            Operator::Mul => write!(f, "*"),
            Operator::Div => write!(f, "/"),
            Operator::Mod => write!(f, "%"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{parser::Parser, scope::IdGenerator, span::Span, std::assert, token::NumberToken};

    use super::*;

    #[test]
    fn test_parse_record_decl() -> Result<(), ParseErr> {
        let tokens = [
            Token::new(TokenKind::Type, Span::default()),
            Token::new(TokenKind::Identifier("Test".to_string()), Span::default()),
            Token::new(TokenKind::Assign, Span::default()),
            Token::new(TokenKind::OpenCurl, Span::default()),
            Token::new(TokenKind::Identifier("a".to_string()), Span::default()),
            Token::new(TokenKind::Identifier("Int".to_string()), Span::default()),
            Token::new(TokenKind::CloseCurl, Span::default()),
        ]
        .to_vec();
        let id = &mut IdGenerator::new(2);
        let diag = &mut Diagnostics::new();
        let mut parser = Parser::new(tokens, id, diag);
        let node = parser.parse()?;
        node.simple_print();
        assert!(false, "Node was Err");
        Ok(())
    }

    #[test]
    fn test_parse_let_binding() -> Result<(), ParseErr> {
        let tokens = [
            TokenKind::Let,
            TokenKind::Identifier("Test".to_string()),
            TokenKind::Assign,
            TokenKind::Number(NumberToken {
                value: "1".to_string(),
                prefix: None,
                suffix: None,
            }),
        ]
        .map(|kind| Token::new(kind, Span::default()))
        .to_vec();

        let id = &mut IdGenerator::new(2);
        let diag = &mut Diagnostics::new();
        let mut parser = Parser::new(tokens, id, diag);
        let node = parser.parse()?;
        node.simple_print();
        assert!(false, "Node was Err");
        Ok(())
    }
}
