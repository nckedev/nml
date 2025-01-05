mod diagnostics;
mod identifier;
mod lexer;
mod log;
mod parser;
mod pos;
mod scope;
mod source_char;
mod span;
mod std;
mod stream;
mod table;
mod token;

use ::std::{
    cell::RefCell,
    fs,
    rc::{Rc, Weak},
};

use crate::parser::*;
use lexer::{Lexer, LexerErr};
use log::Log;
use scope::{IdGenerator, NextScopeId};
use token::Token;

/// notes
/// 1 lexing, strings of text -> list of tokens
/// 2 parsing, list of tokens -> ast
/// 3 analysis -> ast -> correct ast (report compiler errors from here )
/// backend
/// 4 lowering -> correct ast -> byte code

struct FileTokensMap<'a> {
    filename: String,
    tokens: Vec<&'a Token>,
}

struct Args {
    command: String,
    command_arg: String,
}

fn main() -> Result<(), LexerErr> {
    // testing trees
    let root = Tree::new();
    root.add_child(AstNode::Expr { ident: 2 });
    // end of testing

    let paths = fs::read_dir("./nml_std/prelude/").unwrap();
    for p in paths {
        println!("{p:?}");
    }
    let mut id_generator = IdGenerator::new(0);

    println!("tokenizing");
    let test_str1 = "let abc = 123 + 1 * 3";
    // let test_str1 = "mod test\nlet a = 123 + 11 * 2 +3";

    let mut t = Lexer::new(test_str1);
    let tokens = t.tokenize()?;

    for t in &tokens {
        println!("{t}")
    }

    let mut p = Parser::new(tokens, &mut id_generator);
    match p.parse() {
        Ok(x) => {
            Log::debug("printing tree");
            x.print()
        }
        Err(x) => Log::error(format!("{:?}", x).as_str()),
    }
    // if let Ok(r) = p.parse() {
    //     r.print();
    // println!(" tree: {:?}", r)

    let diagnostics = p.get_diagnostics();
    println!("diagnostics: ");
    for d in diagnostics.iter() {
        println!("{d}");
    }

    Ok(())
}

enum AstNode {
    Module { ident: u32 },
    Expr { ident: u32 },
}

trait Test {
    fn test(&self);
}

impl Test for AstNode {
    fn test(&self) {
        match self {
            AstNode::Module { ident } => todo!(),
            AstNode::Expr { ident } => todo!(),
        }
    }
}

struct Tree {
    value: AstNode,
    id: u64,
    scope: u64,
    parent: Option<Weak<RefCell<Tree>>>,
    children: Vec<Rc<RefCell<Tree>>>,
}

impl Tree {
    pub fn new(root: AstNode) -> Rc<RefCell<Self>> {
        match root {
            AstNode::Module { ident } => Rc::new(RefCell::new(Self {
                value: root,
                id: 1,
                scope: 2,
                parent: None,
                children: vec![],
            })),
            _ => panic!("only a module can be a root "),
        }
    }
}

trait AddChild {
    fn add_child(&self, child: AstNode);
}

impl AddChild for Rc<RefCell<Tree>> {
    fn add_child(&self, child: AstNode) {
        let ast = Tree {
            value: child,
            id: 2, //TODO: generated
            scope: 3,
            parent: Some(Rc::downgrade(self)),
            children: vec![],
        };
        // push the child
        self.borrow_mut().children.push(Rc::new(RefCell::new(ast)));
    }
}

trait Traverse {
    ///check if a parent has id, including checking self
    fn find_parent(&self, v: u64) -> bool;
    fn has_children(&self) -> bool;
    fn walk_up(&self) -> Option<Rc<RefCell<Tree>>>;
    fn walk_down(&self) -> Option<Rc<RefCell<Tree>>>;
}

impl Traverse for Rc<RefCell<Tree>> {
    fn find_parent(&self, id: u64) -> bool {
        let this = RefCell::borrow(&self);
        if this.id == id {
            return true;
        } else {
            if let Some(p) = this.parent.clone() {
                if let Some(y) = p.upgrade() {
                    let x = y.borrow();
                    if x.id == id {
                        return true;
                    }
                    return Self::find_parent(self, id);
                }
                false
            } else {
                false
            }
        }
    }

    fn has_children(&self) -> bool {
        let ast = self.borrow();
        match ast.children.len() {
            0 => false,
            _ => true,
        }
    }

    fn walk_up(&self) -> Option<Rc<RefCell<Tree>>> {
        if let Some(parent) = self.borrow().parent.clone() {
            return parent.upgrade();
        }
        None
    }

    fn walk_down(&self) -> Option<Rc<RefCell<Tree>>> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        // let r = Tree::new(AstNode::Module { })
    }
}
