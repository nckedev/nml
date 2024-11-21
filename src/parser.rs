use std::fmt::Display;

use crate::{
    diagnostics::{DiagEntry, Diagnostics},
    scope::{Scope, ScopeGenerator},
};
use crate::{
    stream::Stream,
    token::{Token, TokenKind, TokenTrivia},
};

pub struct ParseState {}

pub struct Parser {
    stream: Stream<Token>,

    //TODO: make diagnostics struct
    diagnostics: Diagnostics,
    id_generator: ScopeGenerator,
}

pub struct ParseErr {
    id: String,
    file: String,
    module: String,
    row: usize,
    col: usize,
    len: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        //strip whitespace
        let t: Vec<Token> = tokens
            .into_iter()
            .filter(|x| {
                x.kind != TokenKind::Trivia(TokenTrivia::Space)
                    && x.kind != TokenKind::Trivia(TokenTrivia::Tab)
            })
            .collect::<Vec<Token>>();
        Self {
            stream: Stream::from(t),
            diagnostics: Diagnostics::new(),
            id_generator: ScopeGenerator::new(),
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
            Node::LetStmt { value: Value, .. } => {
                print!("let ");
                Self::print_tree(Value)
            }
            Node::BinaryExpr {
                left,
                operator,
                right,
            } => {
                Self::print_tree(left);
                print!("{}", operator);
                Self::print_tree(right);
            }
            Node::ConstExpr { value, .. } => print!("{}", value),
            _ => todo!(),
        };
    }

    pub fn parse(&mut self) -> Result<AST, ParseErr> {
        let root = self.id_generator.next();

        //self.print();

        let b = self.parse_stmt();
        let mut ast = AST::new();
        debug::print(&b);
        ast.add(b);

        //
        // //Self::print_tree(&b);
        //
        // // for t in self.tokens {
        // //     let a = self.parse_stmt();
        // //     println!("{:?}", a);
        // // }
        // //
        Ok(ast)
    }

    //parse order -> addative -> multiplicative -> const
    //

    fn parse_addative_expr(&mut self) -> Node {
        let mut left = self.parse_multiplicative_expr();

        while let Some(token) = self.stream.take_if_fn(expect_token::operator_addative) {
            let op = token.kind;
            let right = self.parse_multiplicative_expr();
            left = Node::BinaryExpr {
                left: left.boxed(),
                operator: op.into(),
                right: right.boxed(),
            };
        }

        left
    }

    fn parse_multiplicative_expr(&mut self) -> Node {
        let mut left = self.parse_const_expr();

        while let Some(token) = self
            .stream
            .take_if_fn(expect_token::operator_multiplicative)
        {
            let op = token.kind;
            let right = self.parse_const_expr();
            left = Node::BinaryExpr {
                left: left.boxed(),
                operator: op.into(),
                right: right.boxed(),
            };
        }

        left
    }

    fn parse_const_expr(&mut self) -> Node {
        let Some(token) = self.stream.take() else {
            self.diagnostics
                .push(DiagEntry::message_only(String::from("no token in stream")));
            return Node::Invalid;
        };

        match token.kind {
            TokenKind::Number(x) => Node::ConstExpr {
                scope: self.id_generator.next(),
                value: x.value,
            },
            x => {
                self.diagnostics
                    .push_expected_token_missmatch(&x, "number".into(), &token.span);
                Node::Invalid
            }
        }
    }

    //statements
    //let binding
    //fn declr
    //type declr
    //if statement
    //for loop
    //return
    fn parse_stmt(&mut self) -> Node {
        let Some(token) = self.stream.take() else {
            return Node::EOF;
        };

        debug::print(&token);

        let res = match token.kind {
            TokenKind::Let => {
                if let Some(token) = self.stream.take() {
                    if let TokenKind::Identifier(ident) = token.kind {
                        Node::LetStmt {
                            scope: self.id_generator.next(),
                            ident,
                            value: Box::new(self.parse_addative_expr()),
                        }
                    } else {
                        self.diagnostics.push_expected_token_missmatch(
                            &token.kind,
                            "identifier".to_string(),
                            &token.span,
                        );
                        self.parse_stmt()
                    }
                } else {
                    Node::EOF
                }
            }
            TokenKind::Module => {
                if let Some(token) = self.stream.take() {
                    if let TokenKind::Identifier(i) = token.kind {
                        Node::ModuleDeclr {
                            scope: self.id_generator.next(),
                            ident: i,
                            body: vec![Box::new(self.parse_stmt())],
                        }
                    } else {
                        self.diagnostics.push_expected_token_missmatch(
                            &token.kind,
                            "identifier".to_string(),
                            &token.span,
                        );
                        self.parse_stmt()
                    }
                } else {
                    Node::EOF
                }
            }
            TokenKind::Trivia(TokenTrivia::EOL) => self.parse_stmt(),
            TokenKind::Trivia(TokenTrivia::EOF) => Node::EOF,
            x => {
                self.diagnostics
                    .push(DiagEntry::empty("invalid token".to_string()));
                self.parse_stmt()
            }
        };
        res
    }
}

fn expect(t: TokenKind, n: TokenKind) -> bool {
    t == n
}

struct MetaData {
    id: String,
    attributes: Option<String>,
    type_info: TypeInfo,
    scope: Scope,
    loc: Location,
}

struct TypeInfo {
    access_level: String,
    _type: String,
}

struct Location {
    file: String,
    module: String,
    row: i32,
    col: i32,
}

// !!! expression is something that evaluates to a value
#[derive(Debug)]
pub enum Node {
    VariableAccess,
    FunctionCall,
    MethodCall,

    //stmt
    BlockStmt,
    UseStmt {},
    ModuleDeclr {
        scope: Scope,
        ident: String,
        body: Vec<Box<Node>>,
    },
    LetStmt {
        scope: Scope,
        ident: String,
        value: Box<Node>,
    },

    //expr
    IfExpr,
    MatchExpr,
    ConstExpr {
        scope: Scope,
        value: String,
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
    fn boxed(self) -> Box<Node> {
        Box::new(self)
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let _ = f;
        match self {
            Node::VariableAccess => todo!(),
            Node::FunctionCall => todo!(),
            Node::MethodCall => todo!(),
            Node::BlockStmt => todo!(),
            Node::UseStmt {} => todo!(),
            Node::ModuleDeclr { .. } => todo!(),
            Node::LetStmt { .. } => todo!(),
            Node::ConstExpr { .. } => todo!(),
            Node::BinaryExpr { .. } => todo!(),
            Node::UnaryExpr => todo!(),
            Node::EOF => todo!(),
            Node::IfExpr => todo!(),
            Node::MatchExpr => todo!(),
            Node::Invalid => todo!(),
            Node::Empty => todo!(),
            Node::BooleanExpr { .. } => todo!(),
        }
    }
}

#[derive(Debug)]
pub(crate) struct AST {
    list: Vec<Box<Node>>,
}

impl AST {
    pub fn new() -> Self {
        Self { list: Vec::new() }
    }

    pub fn add(&mut self, node: Node) {
        self.list.push(Box::new(node))
    }

    pub fn print(&mut self) {
        println!("{:?}", &self.list)
    }
}

#[derive(Debug)]
pub(crate) struct ModuleDeclr {
    name: String,
    root: Option<Vec<Box<Node>>>,
}

#[derive(Debug)]
struct ConstExpr {
    value: String,
}
#[derive(Debug)]
struct BinaryExpr {
    left: Box<Node>,
    operator: Operator,
    right: Box<Node>,
}

#[derive(Debug)]
struct VariableDeclr {
    name: String,
    value: Box<Node>,
}

#[derive(Debug)]
enum IdType {
    ScopeId,
    FunctionId,
    MethtodId,
    VariableId,
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

mod expect_token {
    use crate::token::Token;
    use crate::token::TokenKind;

    pub fn any(t: &Token) -> bool {
        true
    }

    pub fn const_expr(t: &Token) -> bool {
        match t.kind {
            TokenKind::Number(..) | TokenKind::String(..) => true,
            _ => false,
        }
    }

    pub fn function_call(t: &Token) -> bool {
        // TODO: Impl
        false
    }

    pub fn operator(t: &Token) -> bool {
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

    pub fn assign(t: &Token) -> bool {
        match t.kind {
            TokenKind::Assign => true,
            _ => false,
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
