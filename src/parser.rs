use std::fmt::Display;

use crate::{
    diagnostics::{DiagEntry, Diagnostics},
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
    diagnostics: Diagnostics,
    id_generator: &'a mut IdGenerator,
    type_table: TypeTable,
    // use_table: Vec<String>,
    // scope_table: Vec<String>,
}

#[derive(Debug)]
pub enum ParseErr {
    UnexpectedEndOfFile,
    UnexpectedToken { token: Token, expected: String },
    NotSupported,
    NotYetImplemented,
}

pub trait UnexpectedTokenErr {
    fn unexpected_token(token: Token, message: String) -> Self;
}

impl UnexpectedTokenErr for ParseErr {
    fn unexpected_token(token: Token, message: String) -> Self {
        Self::UnexpectedToken {
            token,
            expected: message,
        }
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, id_generator: &'a mut IdGenerator) -> Self {
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
            diagnostics: Diagnostics::new(),
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

    fn print_tree(node: &Node) {
        let _ = match node {
            Node::LetStmt { expr: value, .. } => Self::print_tree(value),
            Node::BinaryExpr {
                left,
                operator,
                right,
            } => {
                Self::print_tree(left);
                print!("{}", operator);
                Self::print_tree(right);
            }
            Node::ConstExpr { expr: value, .. } => print!("{}", value),
            _ => todo!(),
        };
    }

    pub fn parse(&mut self) -> Result<AST, ParseErr> {
        Log::info("Parsing");
        let root = self.id_generator.next_scope();

        //self.print();

        let b = self.parse_stmt(root);
        let mut ast = AST::new();
        // debug::print(&b);
        ast.add(b?);

        Ok(ast)
    }

    //parse order -> addative -> multiplicative -> const
    //

    fn parse_addative_expr(&mut self) -> Result<Node, ParseErr> {
        let mut left = self.parse_multiplicative_expr()?;

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
            TokenKind::Number(x) => Node::ConstExpr {
                scope: self.id_generator.next_scope(),
                expr: x.value,
            },
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
        let Some(stmt_token) = self.stream.take() else {
            return Err(ParseErr::UnexpectedEndOfFile);
        };

        // debug::print(&token);

        let res = match stmt_token.kind {
            TokenKind::Let => {
                let (ident, span) = self.stream.take_expecting(expected_token::ident)?;

                //take the '=' token
                let _ = self.stream.take_expecting(expected_token::assign)?;

                Node::LetStmt {
                    parent: scope,
                    span: Span::from((stmt_token.span.start, SourceIndex::from((0, 0)))),
                    ident: Identifier::new(ident, span),
                    expr: Box::new(self.parse_addative_expr()?),
                }
            }

            TokenKind::Module => {
                let token = self.stream.take_or(ParseErr::UnexpectedEndOfFile)?;

                let s = self.id_generator.next_scope();
                match token.kind {
                    TokenKind::Identifier(ident) => Node::ModuleDeclr {
                        scope: s,
                        ident,
                        body: vec![Box::new(self.parse_stmt(s)?)],
                    },
                    _ => {
                        self.diagnostics.push_expected_token_missmatch(
                            &token.kind,
                            "identifier".to_string(),
                            &token.span,
                        );
                        self.parse_stmt(s)?
                    }
                }
            }
            TokenKind::Type => {
                // TODO: type declr, and properties can have attributes
                // type ident = struct {
                //    prop_ident1 prop_type1
                //    prop_ident2 prop_type2
                // }

                // take identifier
                let (ident, span) = self.stream.take_expecting(expected_token::ident)?;

                // take the '='
                let _ = self.stream.take_expecting(expected_token::assign)?;

                // take the 'struct | interface | enum'
                let token = self
                    .stream
                    .take_expecting(expected_token::type_classification)?;

                let type_class_body = match token.kind {
                    TokenKind::Struct => self.parse_struct(scope)?,
                    TokenKind::Interface => self.parse_interface(scope)?,
                    TokenKind::Enum => self.parse_enum(scope)?,
                    _ => todo!(),
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

    fn parse_struct(&mut self, scope: ScopeId) -> Result<Node, ParseErr> {
        //pasrse the
        //{
        //  a type
        //  b type
        //}
        //part of a type declr

        let _ = self.stream.take_expecting(expected_token::open_scope)?;
        todo!()
    }
    fn parse_interface(&mut self, scope: ScopeId) -> Result<Node, ParseErr> {
        todo!()
    }
    fn parse_enum(&mut self, scope: ScopeId) -> Result<Node, ParseErr> {
        todo!("enums nyi")
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

// !!! expression is something that evaluates to a value
#[derive(Debug)]
pub enum Node {
    VariableAccess,
    FunctionCall,
    MethodCall,

    //stmt
    TypeDecl {
        type_id: TypeId,
        ident: Identifier,
        body: Box<Node>,
    },
    BlockStmt,
    UseStmt {},
    ModuleDeclr {
        scope: ScopeId,
        ident: String,
        body: Vec<Box<Node>>,
    },
    LetStmt {
        parent: ScopeId,
        span: Span,
        ident: Identifier,
        expr: Box<Node>,
    },

    //expr
    IfExpr,
    MatchExpr,
    ConstExpr {
        scope: ScopeId,
        expr: String,
    },
    BinaryExpr {
        left: Box<Node>,
        operator: Operator,
        right: Box<Node>,
    },
    BooleanExpr {
        left: Box<Node>,
        operator: Operator,
        right: Box<Node>,
    },

    UnaryExpr,
    EOF,

    Invalid,
    Empty,
}

impl Node {
    fn has_parent(&self, id: ScopeId) -> bool {
        match self {
            Node::ModuleDeclr { scope, ident, body } => false,
            _ => false,
        }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let _ = f;
        match self {
            Node::VariableAccess => write!(f, "VariableAccess"),
            Node::FunctionCall => write!(f, "FunctionCall"),
            Node::MethodCall => write!(f, "MethodCall"),
            Node::BlockStmt => write!(f, "BlockStmt"),
            Node::UseStmt {} => write!(f, "UseStmt "),
            Node::ModuleDeclr { .. } => write!(f, "ModuleDeclr"),
            Node::LetStmt { .. } => write!(f, "LetStmt"),
            Node::ConstExpr { .. } => write!(f, "ConstExpr"),
            Node::BinaryExpr { .. } => write!(f, "BinaryExpr"),
            Node::UnaryExpr => write!(f, "UnaryExpr"),
            Node::EOF => write!(f, "EOF"),
            Node::IfExpr => write!(f, "IfExpr"),
            Node::MatchExpr => write!(f, "MatchExpr"),
            Node::Invalid => write!(f, "Invalid"),
            Node::Empty => write!(f, "Empty"),
            Node::BooleanExpr { .. } => write!(f, "BooleanExpr"),
            Node::TypeDecl { .. } => write!(f, "TypeDecl"),
        }
    }
}

#[derive(Debug)]
pub(crate) struct AST {
    nodes: Vec<Box<Node>>,
}

impl AST {
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn add(&mut self, node: Node) {
        self.nodes.push(Box::new(node))
    }

    pub fn print(&self) {
        for node in self.nodes.iter() {
            AST::print_node(node)
        }
    }
    fn print_node(node: &Node) {
        match node {
            Node::TypeDecl {
                type_id,
                ident,
                body,
            } => println!("typedecl"),
            Node::VariableAccess => println!("variableaccess"),
            Node::FunctionCall => println!("function call"),
            Node::MethodCall => println!("method call"),
            Node::BlockStmt => println!("Block statement"),
            Node::UseStmt {} => println!("use statement"),
            Node::ModuleDeclr { scope, ident, body } => {
                println!("module {} body: ", ident);
                for x in body {
                    AST::print_node(x);
                }
            }
            Node::LetStmt {
                parent,
                span,
                ident,
                expr,
            } => {
                print!("Let {} @ {} expr : ", ident, span);
                println!("{:?}", AST::print_node(expr));
            }
            Node::IfExpr => println!("if expr"),
            Node::MatchExpr => println!("match expr"),
            Node::ConstExpr { scope, expr } => print!("{}", expr),
            Node::BinaryExpr {
                left,
                operator,
                right,
            } => {
                print!("(");
                AST::print_node(left);
                print!("{}", operator);
                AST::print_node(right);
                print!(")");
            }
            Node::BooleanExpr {
                left,
                operator,
                right,
            } => println!("boolean expr"),
            Node::UnaryExpr => println!("unary expr"),
            Node::EOF => println!("EOF"),
            Node::Invalid => println!("INVALID EXPR"),
            Node::Empty => println!("EMPTY"),
        }
    }
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

mod expected_token {
    use crate::parser::ParseErr;
    use crate::span::Span;
    use crate::token::Token;
    use crate::token::TokenKind;

    pub fn ident(t: Token) -> Result<(String, Span), ParseErr> {
        match t.kind {
            TokenKind::Identifier(ident) => Ok((ident, t.span)),
            _ => Err(ParseErr::UnexpectedToken {
                token: t,
                expected: "if statement".to_string(),
            }),
        }
    }

    pub fn assign(t: Token) -> Result<(), ParseErr> {
        match t.kind {
            TokenKind::Assign => Ok(()),
            _ => Err(ParseErr::UnexpectedToken {
                token: t,
                expected: "=".to_string(),
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

    pub fn type_classification(t: Token) -> Result<Token, ParseErr> {
        match t.kind {
            TokenKind::Struct | TokenKind::Interface | TokenKind::Enum => Ok(t),
            _ => Err(ParseErr::UnexpectedToken {
                token: t,
                expected: "struct, interface or enum".to_string(),
            }),
        }
    }

    pub fn any(t: Token) -> Result<Token, ParseErr> {
        Ok(t)
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
        match t.kind {
            TokenKind::Plus | TokenKind::Minus | TokenKind::Mul | TokenKind::Div => true,
            _ => false,
        }
    }
    pub fn operator_addative(t: &Token) -> bool {
        match t.kind {
            TokenKind::Plus | TokenKind::Minus => true,
            _ => false,
        }
    }

    pub fn operator_multiplicative(t: &Token) -> bool {
        match t.kind {
            TokenKind::Mul | TokenKind::Div | TokenKind::Mod => true,
            _ => false,
        }
    }

    pub fn identifier(t: &Token) -> bool {
        if let TokenKind::Identifier(_) = t.kind {
            true
        } else {
            false
        }
    }

    #[cfg(test)]
    mod tests {
        use crate::span::Span;

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
}

mod debug {
    use std::fmt::Debug;

    pub fn print(arg: &impl Debug) {
        println!(">> {:#?}", arg);
    }
}
