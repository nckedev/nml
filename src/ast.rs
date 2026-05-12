use std::{fmt::Display, marker::PhantomData};

use crate::{identifier::Identifier, parser::Operator, scope::TypeId, span::Span};

pub struct Typed;
pub struct Untyped;

#[derive(Debug)]
pub(crate) struct Ast<T> {
    pub nodes: Vec<Node>,
    _marker: PhantomData<T>,
}

impl Ast<Untyped> {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            _marker: PhantomData,
        }
    }

    pub fn add(&mut self, node: Node) {
        self.nodes.push(node)
    }

    pub fn print(&self) {
        for node in self.nodes.iter() {
            Ast::print_node(node)
        }
    }

    fn print_node(node: &Node) {
        match node {
            Node::TypeDecl {
                type_id: _,
                ident: _,
                body: _,
            } => println!("typedecl"),
            Node::ModuleDeclr { ident, body } => {
                println!("module {} body: ", ident);
                for x in body {
                    Ast::print_node(x);
                }
            }
            Node::LetStmt { span, ident, expr } => {
                print!("Let  @  expr : ");
                println!("{:?}", Ast::print_node(expr));
            }
            Node::BinaryExpr {
                left,
                operator,
                right,
            } => {
                print!("(");
                Ast::print_node(left);
                print!("{}", operator);
                Ast::print_node(right);
                print!(")");
            }
            Node::BooleanExpr {
                left: _,
                operator: _,
                right: _,
            } => println!("boolean expr"),
            _ => println!("{:?}", node),
        }
    }
}

impl<T> Display for Ast<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}

// !!! expression is something that evaluates to a value
#[derive(Debug)]
pub enum Node {
    Root {
        list: Vec<Node>,
    },
    VariableAccess,
    FunctionCall,
    MethodCall,

    //stmt
    TypeDecl {
        type_id: TypeId,
        ident: Identifier,
        body: Box<Node>,
    },
    RecordFieldDecl {
        name_ident: Identifier,
        // type_ident: Identifier,
    },
    BlockStmt,
    UseStmt {},
    ModuleDeclr {
        ident: String,
        body: Vec<Node>,
    },
    LetStmt {
        span: Span,
        ident: Box<Node>,
        expr: Box<Node>,
    },
    Ident {
        ident: Identifier,
    },
    TypeIdent {
        ident: String,
    },

    //expr
    IfExpr,
    MatchExpr,
    ConstExpr {
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

    Block {
        stmts: Vec<Node>,
        span: Span,
    },

    UnaryExpr,
    EOF,

    Invalid,
    Empty,
}
