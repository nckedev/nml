mod diagnostics;
mod expected_token;
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
mod vm;

use crate::{diagnostics::Diagnostics, parser::*};
use lexer::{Lexer, LexerErr};
use log::Log;
use scope::IdGenerator;
use token::Token;

// notes
// 1 lexing, strings of text -> list of tokens
// 2 parsing, list of tokens -> ast
// 3 analysis -> ast -> correct ast (report compiler errors from here )
// backend
// 4 lowering -> correct ast -> byte code
//
// Lexer (tokens) ->
// Parser (ast) ->
// Scope Resolve /name bind ->
// Typed AST ->
// type check -> ir / bytecode / code gen

struct FileTokensMap<'a> {
    filename: String,
    tokens: Vec<&'a Token>,
}

fn main() -> Result<(), LexerErr> {
    // testing trees
    // let root = Tree::new();
    // root.add_child(AstNode::Expr { ident: 2 });
    // end of testing

    // let paths = fs::read_dir("./nml_std/prelude/").unwrap();
    // for p in paths {
    //     println!("{p:?}");
    // }
    let mut id_generator = IdGenerator::new(0);

    println!("tokenizing");
    let test_str1 = "let abc = 123 + 1 * 3";
    // let test_str1 = "mod test\nlet a = 123 + 11 * 2 +3";
    let mut diagnostics = Diagnostics::new();
    let mut t = Lexer::new(test_str1, &mut diagnostics);
    let tokens = t.tokenize()?;

    for t in &tokens {
        println!("{t}")
    }

    let mut p = Parser::new(tokens, &mut id_generator, &mut diagnostics);
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        // let r = Tree::new(AstNode::Module { })
    }
}
