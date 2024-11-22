mod diagnostics;
mod lexer;
mod parser;
mod pos;
mod scope;
mod source_char;
mod span;
mod std;
mod stream;
mod token;

use crate::parser::*;
use lexer::{Lexer, LexerErr};

/// notes
/// 1 lexing, strings of text -> list of tokens
/// 2 parsing, list of tokens -> ast
/// 3 analysis -> ast -> correct ast (report compiler errors from here )
/// backend
/// 4 lowering -> correct ast -> byte code

fn main() -> Result<(), LexerErr> {
    println!("tokenizing");
    // let test_str1 = "let abc = 123";
    let test_str1 = "mod test\nlet a = 123 + 11 * 2 +3";

    let mut t = Lexer::new(test_str1);
    let tokens = t.tokenize()?;

    // for t in &lot {
    //     println!("{t:?}")
    // }

    println!("AST");
    let mut p = Parser::new(tokens);
    if let Ok(r) = p.parse() {
        println!(" tree: {:?}", r)
    }

    let diagnostics = p.get_diagnostics();
    println!("diagnostics: ");
    for d in diagnostics.iter() {
        println!("{d}");
    }

    Ok(())
}
