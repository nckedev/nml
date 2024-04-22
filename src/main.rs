mod lexer;
mod pos;
mod std;
mod token;

fn main() {
    println!("tokenizing");
    let test_str1 = "let abc = 123";
    let test_str2 = "if a == 12";

    let mut t = lexer::Lexer::new(test_str1);
    let lot = t.tokenize();

    match lot {
        Ok(x) => {
            for t in x {
                println!("{t:?}")
            }
        }
        Err(_) => (),
    }
}
