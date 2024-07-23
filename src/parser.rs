use std::fmt::Display;

use crate::{
    diagnostics::DiagEntry,
    scope::{Scope, ScopeGenerator},
};
use crate::{
    stream::Stream,
    token::{Token, TokenKind, TokenTrivia},
};

pub struct Parser {
    stream: Stream<Token>,
    tree: AST,
    diagnostics: Vec<DiagEntry>,
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
            tree: AST::new(),
            diagnostics: vec![],
            id_generator: ScopeGenerator::new(),
        }
    }

    // fn print(&self) {
    //     for x in &self.stream {
    //         debug::print(&x);
    //     }
    // }

    fn print_tree(node: &Node) {
        let _ = match node {
            Node::VariableDeclr { value: Value, .. } => {
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

    pub fn parse(&mut self) -> Result<(), ParseErr> {
        let root = self.id_generator.next();
        let mut s = Node::ModuleDeclr(ModuleDeclr {
            name: "mod".to_string(),
            root: None,
        });

        self.tree.add(s);

        //self.print();

        let b = self.parse_expr(expect_token::any);

        debug::print(&b);
        //
        // //Self::print_tree(&b);
        //
        // // for t in self.tokens {
        // //     let a = self.parse_stmt();
        // //     println!("{:?}", a);
        // // }
        // //
        Ok(())
    }

    fn parse_expr(&mut self, is_expected: fn(&Token) -> bool) -> Node {
        let Some(token) = self.stream.take() else {
            panic!("no token found")
        };

        debug::print(&token);

        if !is_expected(&token) {
            panic!("unexpected token {:?}", token);
        }

        let res = match token.kind {
            TokenKind::Let => {
                if self.stream.peek_expect(expect_token::identifier) {
                    let Some(ident) = self.stream.take() else {
                        panic!("uknown expr")
                    };
                    let TokenKind::Identifier(name) = ident.kind else {
                        panic!("uknown expr")
                    };
                    let _ = self.stream.take_if_fn(expect_token::assign);
                    Node::VariableDeclr {
                        scope: self.id_generator.next(),
                        ident: name,
                        value: Box::new(self.parse_expr(expect_token::any)),
                    }
                } else {
                    println!("eof");
                    Node::EOF
                }
            }
            TokenKind::Number(x) => {
                // TODO: convert, check type etc.
                if self.stream.peek_expect(expect_token::operator) {
                    let Some(op) = self.stream.take() else {
                        panic!("operator???")
                    };
                    Node::BinaryExpr {
                        left: Node::ConstExpr {
                            scope: self.id_generator.next(),
                            value: x.value,
                        }
                        .boxed(),
                        operator: Operator::from(op.kind),
                        right: Box::new(self.parse_expr(expect_token::any)),
                    }
                } else {
                    Node::ConstExpr {
                        scope: self.id_generator.next(),
                        value: x.value,
                    }
                }
            }
            _ => {
                self.diagnostics
                    .push(DiagEntry::empty("invalid token".to_string()));
                panic!("asdf")
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

#[derive(Debug)]
pub enum Node {
    VariableAccess,
    FunctionCall,
    MethodCall,
    //stmt
    BlockStmt,
    ModuleDeclr(ModuleDeclr),
    VariableDeclr {
        scope: Scope,
        ident: String,
        value: Box<Node>,
    },
    //expr
    ConstExpr {
        scope: Scope,
        value: String,
    },
    BinaryExpr {
        left: Box<Node>,
        operator: Operator,
        right: Box<Node>,
    },

    UnaryExpr,
    EOF,
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
            Node::ModuleDeclr(_) => todo!(),
            Node::VariableDeclr { .. } => todo!(),
            Node::ConstExpr { .. } => todo!(),
            Node::BinaryExpr { .. } => todo!(),
            Node::UnaryExpr => todo!(),
            Node::EOF => todo!(),
        }
    }
}

#[derive(Debug)]
struct AST {
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

    pub fn operator(t: &Token) -> bool {
        match t.kind {
            TokenKind::Plus | TokenKind::Minus | TokenKind::Mul | TokenKind::Div => true,
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
        use crate::token::TokenSpan;

        use super::*;

        #[test]
        fn expect_identifier() {
            let a = Token::new(TokenKind::Identifier("test".into()), TokenSpan::empty());

            assert!(identifier(&a));
        }
        #[test]
        fn expect_not_identifier() {
            let a = Token::new(TokenKind::Plus, TokenSpan::empty());

            assert!(!identifier(&a));
        }
    }
}

mod debug {
    use std::fmt::Debug;

    pub fn print(arg: &impl Debug) {
        println!("{:#?}", arg);
    }
}
