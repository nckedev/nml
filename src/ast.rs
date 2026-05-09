use std::{fmt::Display, marker::PhantomData};

use crate::{
    identifier::Identifier,
    parser::Operator,
    scope::{ScopeId, TypeId},
    span::Span,
};

pub struct Typed;
pub struct Untyped;

#[derive(Debug)]
pub(crate) struct Ast<T> {
    nodes: Vec<Node>,
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

    pub fn simple_print(&self) {
        for node in &self.nodes {
            Self::simple_print_inner(node, 0);
        }
    }

    pub fn simple_print_inner(node: &Node, depth: u32) {
        let tabs = "\t".repeat(depth as usize);
        match node {
            Node::TypeDecl { ident, body, .. } => {
                println!("{}{}", tabs, node);
                Self::simple_print_inner(body, depth + 1);
            }
            Node::LetStmt { ident, expr, .. } => {
                println!("{}{}", tabs, node);
                Self::simple_print_inner(expr, depth + 1);
            }
            Node::ConstExpr { expr, .. } => {
                println!("{}{} {}", tabs, node, expr);
            }
            _ => println!("{}{}", tabs, node),
        }
    }

    pub fn print(&self) {
        for node in self.nodes.iter() {
            Ast::print_node(node)
        }
    }

    fn print_node(node: &Node) {
        match node {
            Node::TypeDecl {
                type_id,
                ident,
                body,
            } => println!("typedecl"),
            Node::ModuleDeclr { ident, body } => {
                println!("module {} body: ", ident);
                for x in body {
                    Ast::print_node(x);
                }
            }
            Node::LetStmt { span, ident, expr } => {
                print!("Let {} @ {} expr : ", ident, span);
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
                left,
                operator,
                right,
            } => println!("boolean expr"),
            _ => println!("{}", node),
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
        ident: Identifier,
        expr: Box<Node>,
    },
    Ident {
        ident: String,
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

impl Node {
    fn has_parent(&self, id: ScopeId) -> bool {
        match self {
            Node::ModuleDeclr { ident, body } => false,
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
            Node::LetStmt { ident, .. } => write!(f, "LetStmt {}", ident),
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
            Node::RecordFieldDecl { .. } => write!(f, "RecordFieldDecl"),
            Node::Ident { ident } => write!(f, "Ident {}", ident),
            Node::TypeIdent { ident } => write!(f, "TypeIdent {}", ident),
            Node::Block { .. } => write!(f, "Block"),
        }
    }
}
