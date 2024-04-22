use crate::pos::Pos;
use crate::token::Token;
use crate::token::TokenTrivia;
use crate::token::TokenType;

pub struct Lexer {
    code: Vec<char>,
    current: u32,
    length: u32,
    max: u32,
    pos: Pos,
}

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
            code: chars,
            current: 0,
            length: len as u32,
            pos: Pos::new(1, 1, 1),
            max: len as u32 - 1,
        }
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

                    list.push(Token::new(token_type, self.pos()));
                }
                '0'..='9' => {
                    let nr_arr = self.take_until(|x| !(is_number(x) || x == '.'), true);
                    let mut mul = 1;
                    println!("{:?}", nr_arr);

                    if is_valid_float(&nr_arr) {}

                    let nr = nr_arr
                        .into_iter()
                        .map(|c| {
                            let a = (c as i64 - '0' as i64) * mul;
                            mul *= 10;
                            println!("{a:?}");
                            return a;
                        })
                        .sum();
                    list.push(Token::new(TokenType::IntNumber(nr), self.pos()));
                }
                '_' => list.push(Token::new(TokenType::Discard, self.pos())),
                '=' => {
                    if self.next_is('=') {
                        list.push(Token::new(TokenType::Eq, self.pos()));
                    } else if self.next_is('>') {
                        list.push(Token::new(TokenType::Lambda, self.pos()));
                    } else {
                        list.push(Token::new(TokenType::Assign, self.pos()));
                    }
                }
                '>' => {
                    if self.next_is('=') {
                        list.push(Token::new(TokenType::GtEq, self.pos()));
                    } else {
                        list.push(Token::new(TokenType::Gt, self.pos()));
                    }
                }
                '<' => {
                    if self.next_is('=') {
                        list.push(Token::new(TokenType::LtEq, self.pos()));
                    } else {
                        list.push(Token::new(TokenType::Lt, self.pos()));
                    }
                }
                '-' => {
                    list.push(Token::new(TokenType::Minus, self.pos()));
                }
                '+' => {
                    list.push(Token::new(TokenType::Plus, self.pos()));
                }
                '*' => {
                    list.push(Token::new(TokenType::Mul, self.pos()));
                }
                '/' => {
                    list.push(Token::new(TokenType::Div, self.pos()));
                }
                '.' => {
                    if self.next_is('.') {
                        if self.next_is('=') {
                            // inclusive rage ..=
                            list.push(Token::new(TokenType::Range(true), self.pos()));
                        } else {
                            // range ..
                            list.push(Token::new(TokenType::Range(true), self.pos()));
                        }
                    } else {
                        // method accessor, or whatever its called
                        list.push(Token::new(TokenType::MethodAccessor, self.pos()));
                    }
                }
                //string and char
                '"' => {}
                '\'' => {}
                //whitespace
                '\n' => {
                    self.pos.line += 1;
                    self.pos.start = 1;
                    list.push(Token::new(
                        TokenType::Trivia(TokenTrivia::Newline),
                        self.pos(),
                    ));
                }
                '\t' => {
                    list.push(Token::new(TokenType::Trivia(TokenTrivia::Tab), self.pos()));
                }
                ' ' => {
                    list.push(Token::new(
                        TokenType::Trivia(TokenTrivia::Space),
                        self.pos(),
                    ));
                }

                //attribute
                '@' => {
                    todo!()
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

    fn take_number(&mut self) -> Box<Vec<char>> {
        let mut vec = vec![];
        let mut is_float = false;
        let mut found_dot = false;

        while let Some(ch) = self.current() {
            match ch {
                'f' => todo!(),
                '.' if !found_dot => todo!(),
                '.' if found_dot => todo!(),
                '0'..='9' => vec.push(ch),
                _ => break,
            };
        }
        Box::new(vec)
    }

    fn skip_until(&mut self, predicate: fn(char) -> bool) -> () {
        todo!();
    }

    fn next(&mut self) -> Option<char> {
        if self.max == self.current {
            None
        } else {
            self.current += 1;
            Some(self.code[self.current as usize])
        }
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

    /// steps back and returns that character
    fn prev(&mut self) -> Option<char> {
        if self.current > 0 {
            self.current -= 1;
            Some(self.code[self.current as usize])
        } else {
            self.current = 0;
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

    fn next_is_trivia(&self) -> bool {
        if let Some(ch) = self.peek() {
            return is_trivia(ch);
        }
        false
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
    todo!()
}
fn convert_to_float(nr: &Vec<char>) -> f64 {
    2.1
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
        "interface" => TokenType::Interface,
        _ => TokenType::Identifier(str.to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn is_float_is_true() {
        let nr = "10.1".chars().collect();
        assert!(is_valid_float(&nr));
    }
    #[test]
    fn is_float_is_false() {
        let nr = vec!['1', '0'];
        assert!(!is_valid_float(&nr));
    }

    fn setup_lexer_test(code: &str) -> Vec<TokenType> {
        let mut l = Lexer::new(code);
        match l.tokenize() {
            Ok(value) => value.iter().map(|x| x.token_type.clone()).collect(),
            Err(_) => vec![],
        }
    }

    const SPC: TokenType = TokenType::Trivia(TokenTrivia::Space);

    #[test]
    fn lexer_parse_let_binding() {
        use TokenType::*;

        let left = setup_lexer_test("let a = 1");
        let right = vec![
            Let,
            SPC,
            Identifier("a".to_string()),
            SPC,
            Assign,
            SPC,
            IntNumber(1),
        ];
        assert_eq!(left, right);
    }

    #[test]
    fn lexer_parse_if_statement() {
        use TokenType::*;

        let left = setup_lexer_test("if a >= b do a + b end");
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
            End,
        ];
        assert_eq!(left, right);
    }
    #[test]
    fn lexer_parse_if_else_statement() {
        use TokenType::*;

        let left = setup_lexer_test("if a >= b do a + b else a - b end");
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

        let left = setup_lexer_test("0..10");
        let right = vec![IntNumber(1), Range(false), IntNumber(10)];
        assert_eq!(left, right);
    }

    #[test]
    fn lexer_parse_range_inclusive_operator() {
        use TokenType::*;

        let left = setup_lexer_test("0..=10");
        let right = vec![IntNumber(1), Range(true), IntNumber(10)];
        assert_eq!(left, right);
    }
}
