mod ast;
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
#[cfg(test)]
pub mod test_utils;
mod token;
mod vm;

use ::std::fs;

use crate::{
    diagnostics::Diagnostics,
    parser::*,
    token::{TokenKind, TokenTrivia},
};
use lexer::LexerErr;
use log::Log;
use scope::IdGenerator;
use token::Token;
use tracing::instrument;
use tracing_subscriber::{EnvFilter, fmt::format::FmtSpan};

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

#[derive(clap::Parser, Debug)]
#[command(version)]
struct Args {
    #[command(subcommand)]
    commands: Option<Commands>,
}

#[derive(clap::Subcommand, Debug)]
enum Commands {
    Run {
        #[arg(short, long, value_name = "LEVEL")]
        log: Option<String>,
        name: Option<String>,
    },
    Build,
    Fmt,
    Clean,
    Check,
    Lsp,
}

fn main() -> Result<(), LexerErr> {
    let args = <Args as clap::Parser>::parse();
    println!("{:?}", args);

    match &args.commands {
        Some(Commands::Run { log, name }) => {
            if let Some(level) = log {
                tracing_subscriber::fmt()
                    .with_env_filter(EnvFilter::new(level))
                    .with_span_events(FmtSpan::ENTER | FmtSpan::EXIT)
                    .init();
            }
            run()
        }
        None => todo!(),
        _ => todo!(),
    }
    // testing trees
    // let root = Tree::new();
    // root.add_child(AstNode::Expr { ident: 2 });
    // end of testing

    // let paths = fs::read_dir("./nml_std/prelude/").unwrap();
    // for p in paths {
    //     println!("{p:?}");
    // }
    // let mut id_generator = IdGenerator::new(0);
    //
    // println!("tokenizing");
    // let test_str1 = "let abc = 123 + 1 * 3";
    // // let test_str1 = "mod test\nlet a = 123 + 11 * 2 +3";
    // let mut diagnostics = Diagnostics::new();
    // let tokens = lexer::tokenize(test_str1, &mut diagnostics)?;
    //
    // for t in &tokens {
    //     println!("{t}")
    // }
    //
    // let t = tokens.into_iter().filter(|x| {
    //     x.kind != TokenKind::Trivia(TokenTrivia::Space)
    //         && x.kind != TokenKind::Trivia(TokenTrivia::Tab)
    // });
    //
    // let mut p = Parser::new(t, &mut id_generator, &mut diagnostics);
    // match p.parse() {
    //     Ok(x) => {
    //         Log::debug("printing tree");
    //         x.print()
    //     }
    //     Err(x) => Log::error(format!("{:?}", x).as_str()),
    // }
    // // if let Ok(r) = p.parse() {
    // //     r.print();
    // // println!(" tree: {:?}", r)
    //
    // diagnostics.print();

    Ok(())
}

#[instrument(skip_all)]
fn run() {
    let mut id_generator = IdGenerator::new(0);

    let file = "testfile.nml";

    let Ok(str) = fs::read_to_string(file) else {
        println!("Could not read file : {}", file);
        return;
    };

    println!("{str:?}");

    let mut diagnostics = Diagnostics::new();
    let Ok(tokens) = lexer::tokenize(&str, &mut diagnostics) else {
        println!("Failed to parse: \n",);
        return;
    };

    let t = tokens.into_iter().filter(|x| {
        x.kind != TokenKind::Trivia(TokenTrivia::Space)
            && x.kind != TokenKind::Trivia(TokenTrivia::Tab)
    });
    // println!("parsing {:#?}", t);
    let mut p = Parser::new(t, &mut id_generator, &mut diagnostics);
    match p.parse() {
        Ok(x) => {
            Log::debug("printing tree");
            x.print()
        }
        Err(x) => Log::error(format!("{:?}", x).as_str()),
    }

    diagnostics.print();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        // let r = Tree::new(AstNode::Module { })
    }
}
