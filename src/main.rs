mod lexer;
mod parser;
mod pos;
mod scope;
mod std;
mod stream;
mod token;
use lexer::LexerErr;

use crate::parser::*;

fn main() -> Result<(), LexerErr> {
    println!("tokenizing");
    let test_str1 = "let abc = 123 + 1 * 2 +3";

    let mut t = lexer::Lexer::new(test_str1);
    let lot = t.tokenize()?;

    for t in &lot {
        println!("{t:?}")
    }

    println!("AST");
    let mut p = Parser::new(lot);
    p.parse();

    Ok(())
}
