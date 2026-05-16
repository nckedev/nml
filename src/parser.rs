use std::fmt::{Debug, Display};

use crate::{
    ast::{Ast, Node, NodeKind, Untyped},
    diagnostics::{DiagEntry, DiagSeverity::Error, Diagnostics},
    expected_token,
    identifier::Identifier,
    scope::{IdGenerator, ScopeId, TypeId},
    span::Span,
};
use crate::{
    stream::Stream,
    token::{Token, TokenKind, TokenTrivia},
};

pub struct Parser<'a, T, I>
where
    I: Iterator<Item = T>,
    T: Clone + Debug + PartialEq,
{
    stream: Stream<T, I>,
    diagnostics: &'a mut Diagnostics,
    id_generator: &'a mut IdGenerator,
    // use_table: Vec<String>,
    // scope_table: Vec<String>,
}

#[derive(Debug)]
pub enum ParseErr {
    UnexpectedEndOfFile,
    UnexpectedToken { token: Token, expected: String },
    NotSupported,
    Custom(&'static str),
    NotYetImplemented,
}

impl ParseErr {
    pub fn unexpected_token(actual: Token, expected: impl Into<String>) -> Self {
        Self::UnexpectedToken {
            token: actual,
            expected: expected.into(),
        }
    }
}

pub trait MakeDiagnostics<T> {
    fn make_diagnostics(self, diagnostics: &mut Diagnostics) -> Result<T, ParseErr>;
}

impl<T> MakeDiagnostics<T> for Result<T, ParseErr> {
    fn make_diagnostics(self, _diagnostics: &mut Diagnostics) -> Result<T, ParseErr> {
        self
    }
}

pub trait EndOfStream {
    fn end_of_stream() -> Self;
}

impl EndOfStream for ParseErr {
    fn end_of_stream() -> Self {
        Self::UnexpectedEndOfFile
    }
}

impl<O, E> EndOfStream for Result<O, E>
where
    E: EndOfStream,
{
    fn end_of_stream() -> Self {
        Err(E::end_of_stream())
    }
}

impl<T> EndOfStream for Option<T> {
    fn end_of_stream() -> Self {
        None
    }
}

pub trait IsSuccess {
    fn success(&self) -> bool;
}

impl<T> IsSuccess for Option<T> {
    fn success(&self) -> bool {
        self.is_some()
    }
}
impl<O, E> IsSuccess for Result<O, E>
where
    E: EndOfStream,
{
    fn success(&self) -> bool {
        self.is_ok()
    }
}

impl<'a, I> Parser<'a, Token, I>
where
    I: Iterator<Item = Token>,
{
    pub fn new(
        tokens: I,
        id_generator: &'a mut IdGenerator,
        diagnostics: &'a mut Diagnostics,
    ) -> Self {
        //strip whitespace
        Parser {
            stream: Stream::new(tokens),
            diagnostics,
            id_generator,
        }
    }

    pub fn parse(&mut self) -> Result<Ast<Untyped>, ParseErr> {
        //self.print();

        let mut ast = Ast::new();

        // while let Ok(stmt) = self.parse_stmt() {
        //     ast.add(stmt);
        // }

        loop {
            match self.parse_stmt() {
                Ok(v) => ast.add(v),
                Err(ParseErr::UnexpectedEndOfFile) => break,
                Err(e) => {
                    eprintln!("{:?}", e);
                    self.stream.take();
                }
            }
        }

        Ok(ast)
    }

    //parse order -> addative -> multiplicative -> const
    //

    fn parse_addative_expr(&mut self) -> Result<Node, ParseErr> {
        let mut left = self.parse_multiplicative_expr()?;

        let _t = self.stream.take_expecting(|t| {
            expected_token::token_kind(t, &|kind| matches!(kind, TokenKind::Number(..)))
        });

        while let Some(token) = self.stream.take_if_fn(expected_token::operator_addative) {
            let op = token.kind;
            let right = self.parse_multiplicative_expr()?;
            left = Node {
                span: Span::default(),
                kind: NodeKind::BinaryExpr {
                    left: Box::new(left),
                    operator: op.into(),
                    right: Box::new(right),
                },
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
            left = Node {
                span: Span::default(),
                kind: NodeKind::BinaryExpr {
                    left: Box::new(left),
                    operator: op.into(),
                    right: Box::new(right),
                },
            };
        }

        Ok(left)
    }

    fn parse_const_expr(&mut self) -> Result<Node, ParseErr> {
        let Some(token) = self.stream.take() else {
            return Err(ParseErr::UnexpectedEndOfFile);
        };

        let res = match token.kind {
            TokenKind::Number(x) => Node {
                span: Span::default(),
                kind: NodeKind::ConstExpr { expr: x.value },
            },
            TokenKind::Identifier(_ident) => {
                // TODO: variable lookup
                todo!()
            }
            // TODO: function call?
            x => {
                self.diagnostics
                    .push_expected_token_missmatch(&x, "number".into(), &token.span);
                println!("invalid 2 {:?}", x);
                Node {
                    span: Span::default(),
                    kind: NodeKind::Invalid,
                }
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
    fn parse_stmt(&mut self) -> Result<Node, ParseErr> {
        // let Some(stmt_token) = self.stream.take() else {
        //     return Err(ParseErr::UnexpectedEndOfFile);
        // };

        let stmt = self.stream.peek_expecting(expected_token::any)?;

        // debug::print(&token);

        let res = match stmt.kind {
            TokenKind::Let => self.parse_let_binding(),
            TokenKind::Module => self.parse_module_decl(),
            TokenKind::Type => self.parse_type_decl(),
            TokenKind::Eol => self.parse_stmt(),
            TokenKind::Eof => Ok(Node {
                span: stmt.span,
                kind: NodeKind::EOF,
            }),
            _x => {
                self.diagnostics
                    .push(DiagEntry::empty("invalid token".to_string()));
                self.parse_stmt()
            }
        };

        match res {
            Err(err) => {
                self.diagnostics.try_push(err);
                self.stream.skip_until(|t| t.kind == TokenKind::Eol);
                self.parse_stmt()
            }
            x => x,
        }
    }

    fn parse_let_binding(&mut self) -> Result<Node, ParseErr> {
        // take and discard the let keyword
        let Token { span: let_span, .. } = self.stream.take_expecting(expected_token::any)?;
        let (ident, ident_span) = self.stream.take_expecting(expected_token::ident)?;

        //take the '=' token
        self.stream.take_expecting(expected_token::assign)?;
        let expr = self.parse_addative_expr()?;

        Ok(Node {
            span: Span::merge(let_span, expr.span),
            kind: NodeKind::LetStmt {
                ident: Box::new(Node {
                    span: ident_span,
                    kind: NodeKind::Ident {
                        ident: Identifier::new(ident),
                    },
                }),
                expr: Box::new(expr),
            },
        })
    }

    fn parse_type_decl(&mut self) -> Result<Node, ParseErr> {
        // type Test = { field1 Int, field2 Str }
        // -----------------------------------------  <- TypeDecl
        //             _____________________________  <- RecordDecl
        //               ___________  ____________    <- RecordFieldDecl
        //
        // TODO: type declr, and properties can have attributes

        // take identifier
        let _type_kw = self.stream.take_or(ParseErr::UnexpectedEndOfFile)?;

        let (ident, span) = self.stream.take_expecting(expected_token::ident)?;

        if ident.chars().next().is_some_and(char::is_lowercase) {
            self.diagnostics
                .push_message(Error, "Type Identifiers need to be Capitalized", span);
        }

        // take the '='
        self.stream.take_expecting(expected_token::assign)?;

        // take the 'struct | interface | enum'
        let body = self.parse_type_decl_body()?;

        Ok(Node {
            span: Span::default(),
            kind: NodeKind::TypeDecl {
                type_id: self.id_generator.next_type(),
                ident: Identifier::new(ident),
                body: Box::new(body),
            },
        })
    }

    fn parse_module_decl(&mut self) -> Result<Node, ParseErr> {
        let _ = self.stream.take();
        let token = self.stream.take_or(ParseErr::UnexpectedEndOfFile)?;

        let _s = self.id_generator.next_scope();
        match token.kind {
            TokenKind::Identifier(ident) => Ok(Node {
                span: Span::default(),
                kind: NodeKind::ModuleDeclr {
                    ident,
                    body: vec![self.parse_stmt()?],
                },
            }),
            _ => {
                self.diagnostics.push_expected_token_missmatch(
                    &token.kind,
                    "identifier".to_string(),
                    &token.span,
                );
                Err(ParseErr::UnexpectedToken {
                    token,
                    expected: "Identifier".to_string(),
                })
            }
        }
    }

    fn parse_type_decl_body(&mut self) -> Result<Node, ParseErr> {
        // parse the { ... } including the brackets/paren/curlies or if it is just an alias like
        // "type MyType = Int" then the Int part should be parsed
        let open_token = self
            .stream
            .take_expecting(expected_token::type_classification)?;

        let mut buf = vec![];

        let kind = match open_token.kind {
            TokenKind::OpenCurl => {
                while let Ok(field) = self.parse_record_field() {
                    buf.push(field);
                }
                NodeKind::RecordDecl {
                    is_open: false,
                    fields: buf,
                }
            }
            TokenKind::OpenBracket => {
                while let Ok(variant) = self.parse_enum_variant() {
                    buf.push(variant);
                }
                NodeKind::EnumDecl {
                    is_open: false,
                    variants: buf,
                }
            }
            TokenKind::OpenParen => {
                while let Ok(member) = self.parse_tuple_member() {
                    buf.push(member);
                }
                NodeKind::TupleDecl { fields: buf }
            }
            // TokenKind::Interface => self.parse_interface(scope)?,
            _ => unreachable!(),
        };

        let close_token = self
            .stream
            .take_expecting(expected_token::opposit_of(&open_token))?;

        let body = Node {
            span: Span::merge(open_token.span, close_token.span),
            kind,
        };

        Ok(body)
    }

    fn parse_record_field(&mut self) -> Result<Node, ParseErr> {
        //{
        //  a type, <- parse this
        //  b type  <- and this
        //}
        // self.stream.peek_expecting(expected_token::ident)?;
        // the name
        let (name_ident, name_span) = self.stream.take_if_expecting(expected_token::ident)?;
        // the type name
        // TODO: This could be a anontype or open enum
        let (type_ident, type_span) = self.stream.take_if_expecting(expected_token::ident)?;

        let _ = self
            .stream
            .take_if_expecting(expected_token::exact_kind(&TokenKind::Separator));

        Ok(Node {
            span: Span::merge(name_span, type_span),
            kind: NodeKind::RecordFieldDecl {
                name_ident: Identifier { value: name_ident },
                type_ident: Identifier { value: type_ident },
            },
        })
    }

    fn parse_enum_variant(&mut self) -> Result<Node, ParseErr> {
        // [A, B, C]
        let (ident, span) = self.stream.take_if_expecting(expected_token::ident)?;
        let _ = self
            .stream
            .take_if_expecting(expected_token::exact_kind(&TokenKind::Separator));

        Ok(Node {
            span,
            kind: NodeKind::EnumVariantDecl {
                name: Identifier { value: ident },
            },
        })
    }

    fn parse_tuple_member(&mut self) -> Result<Node, ParseErr> {
        // (Int, Str)
        let (ident, span) = self.stream.take_if_expecting(expected_token::ident)?;
        let _ = self
            .stream
            .take_if_expecting(expected_token::exact_kind(&TokenKind::Separator));

        Ok(Node {
            span,
            kind: NodeKind::EnumVariantDecl {
                name: Identifier { value: ident },
            },
        })
    }

    fn parse_interface(&mut self) -> Result<Node, ParseErr> {
        todo!()
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
    use crate::{
        parser::Parser,
        scope::IdGenerator,
        span::Span,
        test_utils::SnapshotStr,
        token::{NumberToken, NumberTokenPrefix, NumberTokenSuffix},
    };

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
        let mut parser = Parser::new(tokens.into_iter(), id, diag);
        let node = parser.parse()?;
        insta::assert_snapshot!(node.nodes.snapshot());
        Ok(())
    }

    #[test]
    fn test_parse_record_decl_2_fields() -> Result<(), ParseErr> {
        let tokens = [
            Token::new(TokenKind::Type, Span::default()),
            Token::new(TokenKind::Identifier("Test".to_string()), Span::default()),
            Token::new(TokenKind::Assign, Span::default()),
            Token::new(TokenKind::OpenCurl, Span::default()),
            Token::new(TokenKind::Identifier("a".to_string()), Span::default()),
            Token::new(TokenKind::Identifier("Int".to_string()), Span::default()),
            Token::new(TokenKind::Separator, Span::default()),
            Token::new(TokenKind::Identifier("b".to_string()), Span::default()),
            Token::new(TokenKind::Identifier("Int".to_string()), Span::default()),
            Token::new(TokenKind::CloseCurl, Span::default()),
        ]
        .to_vec();

        let id = &mut IdGenerator::new(2);
        let diag = &mut Diagnostics::new();
        let mut parser = Parser::new(tokens.into_iter(), id, diag);
        let node = parser.parse()?;
        insta::assert_snapshot!(node.nodes.snapshot());
        Ok(())
    }

    #[test]
    fn test_parse_record_decl_3_fields_trailing_sep() -> Result<(), ParseErr> {
        let tokens = [
            Token::new(TokenKind::Type, Span::default()),
            Token::new(TokenKind::Identifier("Test".to_string()), Span::default()),
            Token::new(TokenKind::Assign, Span::default()),
            Token::new(TokenKind::OpenCurl, Span::default()),
            Token::new(TokenKind::Identifier("a".to_string()), Span::default()),
            Token::new(TokenKind::Identifier("Int".to_string()), Span::default()),
            Token::new(TokenKind::Separator, Span::default()),
            Token::new(TokenKind::Identifier("b".to_string()), Span::default()),
            Token::new(TokenKind::Identifier("Int".to_string()), Span::default()),
            Token::new(TokenKind::Separator, Span::default()),
            Token::new(TokenKind::Identifier("c".to_string()), Span::default()),
            Token::new(TokenKind::Identifier("Str".to_string()), Span::default()),
            Token::new(TokenKind::Separator, Span::default()),
            Token::new(TokenKind::CloseCurl, Span::default()),
        ]
        .to_vec();
        let id = &mut IdGenerator::new(2);
        let diag = &mut Diagnostics::new();
        let mut parser = Parser::new(tokens.into_iter(), id, diag);
        let node = parser.parse()?;
        insta::assert_snapshot!(node.nodes.snapshot());
        Ok(())
    }

    #[test]
    fn test_parse_enum_variant_decl() -> Result<(), ParseErr> {
        let tokens = [
            Token::new(TokenKind::Type, Span::default()),
            Token::new(TokenKind::Identifier("Test".to_string()), Span::default()),
            Token::new(TokenKind::Assign, Span::default()),
            Token::new(TokenKind::OpenBracket, Span::default()),
            Token::new(TokenKind::Identifier("A".to_string()), Span::default()),
            Token::new(TokenKind::CloseBracket, Span::default()),
        ]
        .to_vec();
        let id = &mut IdGenerator::new(2);
        let diag = &mut Diagnostics::new();
        let mut parser = Parser::new(tokens.into_iter(), id, diag);
        let node = parser.parse()?;
        insta::assert_snapshot!(node.nodes.snapshot());
        Ok(())
    }

    #[test]
    fn test_parse_enum_variant_decl_2_trailing() -> Result<(), ParseErr> {
        let tokens = [
            Token::new(TokenKind::Type, Span::default()),
            Token::new(TokenKind::Identifier("Test".to_string()), Span::default()),
            Token::new(TokenKind::Assign, Span::default()),
            Token::new(TokenKind::OpenBracket, Span::default()),
            Token::new(TokenKind::Identifier("A".to_string()), Span::default()),
            Token::new(TokenKind::Separator, Span::default()),
            Token::new(TokenKind::Identifier("B".to_string()), Span::default()),
            Token::new(TokenKind::Separator, Span::default()),
            Token::new(TokenKind::CloseBracket, Span::default()),
        ]
        .to_vec();
        let id = &mut IdGenerator::new(2);
        let diag = &mut Diagnostics::new();
        let mut parser = Parser::new(tokens.into_iter(), id, diag);
        let node = parser.parse()?;
        insta::assert_snapshot!(node.nodes.snapshot());
        Ok(())
    }

    #[test]
    fn test_parse_tuple_decl() -> Result<(), ParseErr> {
        let tokens = [
            Token::new(TokenKind::Type, Span::default()),
            Token::new(TokenKind::Identifier("Test".to_string()), Span::default()),
            Token::new(TokenKind::Assign, Span::default()),
            Token::new(TokenKind::OpenParen, Span::default()),
            Token::new(TokenKind::Identifier("Int".to_string()), Span::default()),
            Token::new(TokenKind::CloseParen, Span::default()),
        ]
        .to_vec();
        let id = &mut IdGenerator::new(2);
        let diag = &mut Diagnostics::new();
        let mut parser = Parser::new(tokens.into_iter(), id, diag);
        let node = parser.parse()?;
        insta::assert_snapshot!(node.nodes.snapshot());
        Ok(())
    }

    #[test]
    fn test_parse_tuple_decl_2_trailing() -> Result<(), ParseErr> {
        let tokens = [
            Token::new(TokenKind::Type, Span::default()),
            Token::new(TokenKind::Identifier("Test".to_string()), Span::default()),
            Token::new(TokenKind::Assign, Span::default()),
            Token::new(TokenKind::OpenParen, Span::default()),
            Token::new(TokenKind::Identifier("Int".to_string()), Span::default()),
            Token::new(TokenKind::Separator, Span::default()),
            Token::new(TokenKind::Identifier("Str".to_string()), Span::default()),
            Token::new(TokenKind::Separator, Span::default()),
            Token::new(TokenKind::CloseParen, Span::default()),
        ]
        .to_vec();
        let id = &mut IdGenerator::new(2);
        let diag = &mut Diagnostics::new();
        let mut parser = Parser::new(tokens.into_iter(), id, diag);
        let node = parser.parse()?;
        insta::assert_snapshot!(node.nodes.snapshot());
        Ok(())
    }

    #[test]
    fn test_parse_let_binding() -> Result<(), ParseErr> {
        let tokens = [
            TokenKind::Let,
            TokenKind::Identifier("test".to_string()),
            TokenKind::Assign,
            TokenKind::Number(NumberToken {
                value: "1".to_string(),
                prefix: NumberTokenPrefix::None,
                suffix: NumberTokenSuffix::None,
            }),
        ]
        .map(|kind| Token::new(kind, Span::default()))
        .to_vec();

        let id = &mut IdGenerator::new(2);
        let diag = &mut Diagnostics::new();
        let mut parser = Parser::new(tokens.into_iter(), id, diag);
        let node = parser.parse()?;
        insta::assert_snapshot!(node.nodes.snapshot());
        Ok(())
    }
}
