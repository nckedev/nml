use core::panic;

use crate::pos::Pos;
use crate::stream::Stream;
use crate::token::Token;
use crate::token::TokenTrivia;
use crate::token::TokenType;

pub struct Lexer {
    buffer: Stream<char>,
    code: Vec<char>,
    current: u32,
    length: u32,
    max: u32,
    pos: Pos,
}

#[derive(Debug)]
pub struct LexerErr {
    message: String,
    pos: Pos,
}

impl LexerErr {
    pub fn new(msg: String, line: u32, start: u32, end: u32) -> Self {
        LexerErr {
            message: msg,
            pos: Pos { line, start, end },
        }
    }
}

impl Lexer {
    pub fn new(code: &str) -> Self {
        let chars: Vec<char> = code.chars().collect();
        let len = chars.len();
        Lexer {
            code: chars.clone(),
            buffer: Stream::from(chars),
            current: 0,
            length: len as u32,
            pos: Pos::new(1, 1, 1),
            max: len as u32 - 1,
        }
    }

    fn gen_token(&self, t: TokenType) -> Token {
        Token::new(t, self.pos())
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerErr> {
        let mut list = vec![];

        while let Some(ch) = self.current() {
            self.pos.start = self.current + 1;

            match ch {
                'a'..='z' | 'A'..='Z' => {
                    let lit_arr = self.take_until(|x| x == ' ', true);
                    let str: String = lit_arr.into_iter().collect();
                    let token_type = match_litteral(&str);

                    list.push(self.gen_token(token_type));
                }
                '0'..='9' => {
                    let token_nr = self.take_number();
                    list.push(self.gen_token(token_nr));
                    // let nr_arr = self.take_until(|x| !(is_number(x) || x == '.'), true);
                    // let mut mul = 1;

                    // let nr = nr_arr
                    //     .into_iter()
                    //     .map(|c| {
                    //         let a = (c as i64 - '0' as i64) * mul;
                    //         mul *= 10;
                    //         println!("{a:?}");
                    //         return a;
                    //     })
                    //     .sum();
                    // list.push(self.gen_token(TokenType::IntNumber(nr))));
                }
                '_' => list.push(self.gen_token(TokenType::Discard)),
                '=' => {
                    if self.next_is('=') {
                        list.push(self.gen_token(TokenType::Eq));
                    } else if self.next_is('>') {
                        list.push(self.gen_token(TokenType::Lambda));
                    } else {
                        list.push(self.gen_token(TokenType::Assign));
                    }
                }
                '>' => {
                    if self.next_is('=') {
                        list.push(self.gen_token(TokenType::GtEq));
                    } else {
                        list.push(self.gen_token(TokenType::Gt));
                    }
                }
                '<' => {
                    if self.next_is('=') {
                        list.push(self.gen_token(TokenType::LtEq));
                    } else {
                        list.push(self.gen_token(TokenType::Lt));
                    }
                }
                '-' => {
                    list.push(self.gen_token(TokenType::Minus));
                }
                '+' => {
                    list.push(self.gen_token(TokenType::Plus));
                }
                '*' => {
                    list.push(self.gen_token(TokenType::Mul));
                }
                '/' => {
                    list.push(self.gen_token(TokenType::Div));
                }
                '.' => {
                    if self.next_is('.') {
                        if self.next_is('=') {
                            // inclusive rage ..=
                            list.push(self.gen_token(TokenType::Range(true)));
                        } else {
                            // range ..
                            list.push(self.gen_token(TokenType::Range(false)));
                        }
                    } else {
                        // method accessor, or whatever its called
                        list.push(self.gen_token(TokenType::MethodAccessor));
                    }
                }
                '{' => list.push(self.gen_token(TokenType::OpenCurl)),
                '}' => list.push(self.gen_token(TokenType::CloseCurl)),
                //string and char
                '"' => {}
                '\'' => {}
                //whitespace
                '\n' => {
                    self.pos.line += 1;
                    self.pos.start = 1;
                    list.push(self.gen_token(TokenType::Trivia(TokenTrivia::EOL)));
                }
                '\t' => {
                    list.push(self.gen_token(TokenType::Trivia(TokenTrivia::Tab)));
                }
                ' ' => {
                    list.push(Token::new(
                        TokenType::Trivia(TokenTrivia::Space),
                        self.pos(),
                    ));
                }

                //attribute, @test
                '@' => {
                    //take until space or newline
                    //Todo: attribute with parameters @test(arg1, arg2)
                    let attr_arr = self.take_until(|x| x == ' ' || x == '\n', false);
                    let str: String = attr_arr.into_iter().collect();
                    list.push(self.gen_token(TokenType::Attribute(str)));
                }
                _ => (),
            }
            self.step(1);
        }
        Ok(list)
    }

    fn pos(&self) -> Pos {
        Pos {
            line: self.pos.line,
            start: self.pos.start,
            end: self.current,
        }
    }

    fn take_until(&mut self, predicate: fn(char) -> bool, including: bool) -> Box<Vec<char>> {
        let mut vec = vec![];

        //TODO: tar en för mycket, är nog stepen i tokenizer som gör det
        while let Some(ch) = self.current() {
            if predicate(ch) {
                println!("{}", ch);
                self.step(-1);
                break;
            } else {
                self.step(1);
                vec.push(ch);
            }
        }

        Box::new(vec)
    }

    fn take_number(&mut self) -> TokenType {
        let mut vec = vec![];
        let mut is_float = false;
        let mut found_dot = false;

        while let Some(ch) = self.current() {
            match ch {
                'f' => {
                    is_float = true;
                    break;
                }
                '.' if !found_dot => {
                    found_dot = true;
                    is_float = true;
                    vec.push(ch);
                }
                '.' if found_dot => {
                    self.step(-1);
                    self.step(-1);
                    return TokenType::IntNumber(1);
                }
                '_' => {}
                //'.' if found_dot => todo!(),
                '0'..='9' => vec.push(ch),
                _ => break,
            };
            self.current += 1;
        }
        if is_float {
            return TokenType::FloatNumber(convert_to_float(&vec));
        } else {
            println!("int");
            return TokenType::IntNumber(convert_to_int(&vec));
        }
    }

    fn skip_until(&mut self, predicate: fn(char) -> bool) -> () {
        todo!();
    }

    //returns a character and steps forward 1 step
    fn take(&mut self) -> Option<char> {
        // let test = self.code.get(self.current as usize);
        if self.can_take() {
            let t = Some(self.code[self.current as usize]);
            self.step(1);
            return t;
            // t
        } else {
            None
        }
    }

    fn next_is(&mut self, expected: char) -> bool {
        if self.has_next() && (self.peek() == Some(expected)) {
            self.step(1);
            return true;
        }
        false
    }

    //returns the current character
    fn current(&self) -> Option<char> {
        if self.can_take() {
            let t = Some(self.code[self.current as usize]);
            return t;
        } else {
            None
        }
    }

    //steps <offset> steps forward
    fn step(&mut self, offset: i32) {
        //TODO: Offset ska inte bara vara 1
        if offset > 0 {
            self.current += 1;
        } else if offset < 0 {
            self.current -= 1;
        }
    }

    fn peek(&self) -> Option<char> {
        if self.has_next() {
            return Some(self.code[(self.current + 1) as usize]);
        }
        None
    }

    fn has_next(&self) -> bool {
        self.current <= self.max
    }

    fn can_take(&self) -> bool {
        self.current <= self.max
    }
}

fn is_alpha(c: char) -> bool {
    match c {
        'a'..='z' => true,
        'A'..='Z' => true,
        '_' => true,
        _ => false,
    }
}

fn is_number(c: char) -> bool {
    match c {
        '0'..='9' => true,
        _ => false,
    }
}

fn is_valid_float(nr: &Vec<char>) -> bool {
    let mut count = 0;
    for x in nr {
        if *x == '.' {
            count += 1;
        }
    }
    count == 1
}

fn convert_to_int(nr: &Vec<char>) -> i64 {
    let str: String = nr.iter().collect();
    let nr = str.parse::<i64>();
    match nr {
        Ok(x) => x,
        Err(_) => panic!("inget nummer"),
    }
}
fn convert_to_float(nr: &Vec<char>) -> f64 {
    let str: String = nr.iter().collect();
    let nr = str.parse::<f64>();
    match nr {
        Ok(x) => x,
        Err(_) => panic!("inget nummer"),
    }
}

fn is_valid_int(nr: &Vec<char>) -> bool {
    todo!()
}

fn is_alpha_or_number(c: char) -> bool {
    is_alpha(c) || is_number(c)
}

fn is_trivia(c: char) -> bool {
    match c {
        ' ' | '\t' | '\n' => true,
        _ => false,
    }
}

fn match_litteral(str: &str) -> TokenType {
    match str {
        "let" => TokenType::Let,
        "if" => TokenType::If,
        "do" => TokenType::Do,
        "else" => TokenType::Else,
        "end" => TokenType::End,
        "for" => TokenType::For,
        "mut" => TokenType::Mut,
        "ref" => TokenType::Ref,
        "interface" => TokenType::Interface,
        "trait" => TokenType::Trait,
        "variant" => TokenType::Variant,
        "const" => TokenType::Const,
        _ => TokenType::Identifier(str.to_string()),
    }
}

enum NmlExpr {
    Binary,
    Unary,
    FunctionCall,
}

impl NmlExpr {
    fn from_tokens(t: &Token) -> NmlExpr {
        return NmlExpr::Binary;
    }
}

#[cfg(test)]
mod tests {
    use super::{convert_to_int, *};

    #[test]
    fn test_is_valid_float() {
        let nr = "10.1".chars().collect();
        assert!(is_valid_float(&nr));
    }
    #[test]
    fn test_is_float_is_false() {
        let nr = vec!['1', '0'];
        assert!(!is_valid_float(&nr));
    }

    #[test]
    fn test_convert_to_int() {
        let left = 1i64;
        let right = convert_to_int(&vec!['1']);
        assert_eq!(left, right);

        let left = 23i64;
        let right = convert_to_int(&vec!['2', '3']);
        assert_eq!(left, right);
        let left = 1123i64;
        let right = convert_to_int(&vec!['1', '1', '2', '3']);
        assert_eq!(left, right);
    }

    #[test]
    fn test_convert_to_float() {
        let left = 1f64;
        let right = convert_to_float(&vec!['1', '.', '0']);
        assert_eq!(left, right);

        let left = 10.123f64;
        let right = convert_to_float(&vec!['1', '0', '.', '1', '2', '3']);
        assert_eq!(left, right);

        let left = 11.0f64;
        let right = convert_to_float(&vec!['1', '1', '.']);
        assert_eq!(left, right);
    }

    // lexer integrations test
    const SPC: TokenType = TokenType::Trivia(TokenTrivia::Space);

    fn setup_lexer_test(code: &str, skip_whitespace: bool) -> Vec<TokenType> {
        let mut l = Lexer::new(code);
        match l.tokenize() {
            Ok(value) => value
                .iter()
                .map(|x| x.token_type.clone())
                .filter(|x| if skip_whitespace { *x != SPC } else { true })
                .collect(),
            Err(_) => vec![],
        }
    }

    #[test]
    fn test_number_format_int() {
        use TokenType::*;
        let left = setup_lexer_test("let a = 1_000_000", true);
        let right = vec![Let, Identifier("a".to_string()), Assign, IntNumber(1000000)];
        assert_eq!(left, right);
    }

    #[test]
    fn test_number_format_float() {
        use TokenType::*;
        let left = setup_lexer_test("let a = 1_000_000.001", true);
        let right = vec![
            Let,
            Identifier("a".to_string()),
            Assign,
            FloatNumber(1000000.001),
        ];
        assert_eq!(left, right);
    }

    #[test]
    fn lexer_parse_let_binding() {
        use TokenType::*;

        let left = setup_lexer_test("let a = 1", true);
        let right = vec![Let, Identifier("a".to_string()), Assign, IntNumber(1)];
        assert_eq!(left, right);
    }

    #[test]
    fn lexer_parse_if_statement() {
        use TokenType::*;

        let left = setup_lexer_test("if a >= b do a + b end", true);
        let right = vec![
            If,
            Identifier("a".to_string()),
            GtEq,
            Identifier("b".to_string()),
            Do,
            Identifier("a".to_string()),
            Plus,
            Identifier("b".to_string()),
            End,
        ];
        assert_eq!(left, right);
    }
    #[test]
    fn lexer_parse_if_statement_curl() {
        use TokenType::*;

        let left = setup_lexer_test("if a >= b { a + b }", true);
        let right = vec![
            If,
            Identifier("a".to_string()),
            GtEq,
            Identifier("b".to_string()),
            OpenCurl,
            Identifier("a".to_string()),
            Plus,
            Identifier("b".to_string()),
            CloseCurl,
        ];
        assert_eq!(left, right);
    }
    #[test]
    fn lexer_parse_if_else_statement() {
        use TokenType::*;

        let left = setup_lexer_test("if a >= b do a + b else a - b end", false);
        let right = vec![
            If,
            SPC,
            Identifier("a".to_string()),
            SPC,
            GtEq,
            SPC,
            Identifier("b".to_string()),
            SPC,
            Do,
            SPC,
            Identifier("a".to_string()),
            SPC,
            Plus,
            SPC,
            Identifier("b".to_string()),
            SPC,
            Else,
            SPC,
            Identifier("a".to_string()),
            SPC,
            Minus,
            SPC,
            Identifier("b".to_string()),
            SPC,
            End,
        ];
        assert_eq!(left, right);
    }

    #[test]
    fn lexer_parse_range_operator() {
        use TokenType::*;

        let left = setup_lexer_test("0..10", false);
        let right = vec![IntNumber(1), Range(false), IntNumber(10)];
        assert_eq!(left, right);
    }

    #[test]
    fn lexer_parse_range_inclusive_operator() {
        use TokenType::*;

        let left = setup_lexer_test("0..=10", false);
        let right = vec![IntNumber(1), Range(true), IntNumber(10)];
        assert_eq!(left, right);
    }
}
