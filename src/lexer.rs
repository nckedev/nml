use std::usize;

use crate::source_char::SourceChar;
use crate::source_char::SourceIndex;
use crate::span::Span;
use crate::stream::Stream;
use crate::token::NumberToken;
use crate::token::Token;
use crate::token::TokenError::Unexpected;
use crate::token::TokenKind;
use crate::token::TokenTrivia;

pub struct Lexer {
    stream: Stream<SourceChar>,
}

#[derive(Debug)]
pub struct LexerErr {
    message: String,
    // TODO: add token span
}

impl Lexer {
    pub fn new(code: &str) -> Self {
        let mut sourcechars: Vec<SourceChar> = Vec::with_capacity(code.len());

        //transform chars to SourceChars to get the index of every char
        let mut row = 1 as usize;
        let mut col = 1 as usize;

        for c in code.chars() {
            if c == '\n' {
                row += 1;
                col = 1;
            }
            sourcechars.push(SourceChar {
                ch: c,
                index: SourceIndex { row, col },
            });
            col += 1;
        }

        // insert a space at the last pos so that we can peek from the last char
        // needed to get the index for the last token
        // TODO: detta funkar men fuckar upp testen
        if let Some(last) = sourcechars.last() {
            sourcechars.push(SourceChar {
                ch: ' ',
                index: SourceIndex {
                    row: last.index.row,
                    col: last.index.col + 1,
                },
            });
        }

        // return the lexer with SourceChars
        Lexer {
            stream: Stream::from(sourcechars),
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerErr> {
        let mut tokens = vec![];

        while let Some(v) = self.stream.take() {
            let token_kind = match v {
                // identifier, keyword
                SourceChar {
                    ch: 'a'..='z' | 'A'..='Z',
                    ..
                } => {
                    let litteral = self
                        .stream
                        .take_while_iter(|x| x.is_alpha_or_number())
                        .map(|x| x.ch)
                        .collect::<String>();

                    match_litteral(&(v.ch.to_string() + litteral.as_ref()))
                }
                //number
                SourceChar {
                    ch: '0'..='9',
                    index: _start,
                } => self.take_number(&v),
                //discard _
                SourceChar { ch: '_', .. } => TokenKind::Discard,
                // = or == or =>
                SourceChar { ch: '=', .. } => {
                    if self.stream.peek_and_step_if(SourceChar::from('=')) {
                        TokenKind::Eq
                    } else if self.stream.peek_and_step_if(SourceChar::from('>')) {
                        TokenKind::Lambda
                    } else {
                        TokenKind::Assign
                    }
                }
                // >= or >
                SourceChar { ch: '>', .. } => {
                    match self.stream.peek_and_step_if(SourceChar::from('=')) {
                        true => TokenKind::GtEq,
                        false => TokenKind::Gt,
                    }
                }
                // <= or <
                SourceChar { ch: '<', .. } => {
                    match self.stream.peek_and_step_if(SourceChar::from('=')) {
                        true => TokenKind::LtEq,
                        false => TokenKind::Lt,
                    }
                }
                //arrow or minus
                SourceChar { ch: '-', .. } => {
                    if self.stream.peek_and_step_if(SourceChar::from('>')) {
                        TokenKind::Arrow
                    } else {
                        TokenKind::Minus
                    }
                }
                //math operators
                SourceChar { ch: '+', .. } => TokenKind::Plus,
                SourceChar { ch: '*', .. } => TokenKind::Mul,
                SourceChar { ch: '/', .. } => TokenKind::Div,

                //method accessor or range operator
                SourceChar { ch: '.', .. } => {
                    if self.stream.peek_and_step_if(SourceChar::from('.')) {
                        if self.stream.peek_and_step_if(SourceChar::from('=')) {
                            // inclusive rage ..=
                            TokenKind::InclusiveRange
                        } else {
                            // range ..
                            TokenKind::ExclusiveRange
                        }
                    } else {
                        if self.stream.peek_expect(|x| x.is_number()) {
                            self.take_number(&v)
                        } else {
                            // method accessor, or whatever its called
                            TokenKind::MethodAccessor
                        }
                    }
                }
                SourceChar { ch: ',', .. } => TokenKind::Separator,
                SourceChar { ch: '{', .. } => TokenKind::OpenCurl,
                SourceChar { ch: '}', .. } => TokenKind::CloseCurl,
                SourceChar { ch: '(', .. } => TokenKind::OpenParen,
                SourceChar { ch: ')', .. } => TokenKind::CloseParen,
                //string and char
                SourceChar { ch: '"', .. } => TokenKind::Error(Unexpected(v.ch)),
                SourceChar { ch: '\'', .. } => TokenKind::Error(Unexpected(v.ch)),
                //whitespace
                SourceChar { ch: '\n', .. } => TokenKind::Trivia(TokenTrivia::EOL),
                SourceChar { ch: '\t', .. } => TokenKind::Trivia(TokenTrivia::Tab),
                SourceChar { ch: ' ', .. } => TokenKind::Trivia(TokenTrivia::Space),

                //attribute, @test
                SourceChar { ch: '@', .. } => {
                    if self.stream.peek_expect(SourceChar::is_alpha) {
                        TokenKind::AtMarker
                    } else {
                        TokenKind::Error(Unexpected(v.ch))
                    }

                    // //take until space or newline
                    // //Todo: attribute with parameters @test(arg1, arg2)
                    // let attr_arr = self.stream.take_until(|x| x.ch == ' ' || x.ch == '\n');
                    // let str: String = attr_arr.into_iter().map(|x| x.ch).collect();
                    // TokenKind::Attribute(str)
                }

                //np match
                _ => TokenKind::Error(Unexpected(v.ch)),
            };

            //get the end index by looking at the start index for the next SourceChar
            let end_index = if let Some(t) = self.stream.peek() {
                t.index
            } else {
                SourceIndex { row: 0, col: 0 }
            };

            let token = Token {
                kind: token_kind,
                span: (v.index, end_index).into(),
            };
            tokens.push(token);
        }

        //the last entry will not have a correct span
        let last_index = tokens.len() - 1;
        if let Some(t) = tokens.get_mut(last_index) {
            println!("{:#?}", t);
            t.span.end = (0 as usize, last_index).into();
        }
        Ok(tokens)
    }

    /// Returs an number (int or float) from the stream and advances
    fn take_number(&mut self, sc: &SourceChar) -> TokenKind {
        let mut number_buf: Vec<SourceChar> = vec![];
        let mut suffix_buf: Vec<SourceChar> = vec![];
        let mut has_suffix = false;

        // push the first char that has already been taken by the main loop
        number_buf.push(sc.clone());

        while let Some(v) = self.stream.peek() {
            match v {
                SourceChar {
                    ch: 'a'..='z' | 'A'..='Z',
                    ..
                } => {
                    if let Some(t) = self.stream.take() {
                        suffix_buf.push(t);
                        has_suffix = true;
                    }
                }
                SourceChar { ch: '.', .. } => {
                    // if there is two dots in a row it is a range operator
                    // so return what we have got so far as a IntNumber
                    if self.stream.peek_n_expect(1, |x| x.ch == '.') {
                        break;
                    }
                    number_buf.push(v);
                    self.stream.take();
                }
                SourceChar { ch: '_', index: _ } => {
                    self.stream.take();
                }
                SourceChar {
                    ch: '0'..='9',
                    index: _,
                } if has_suffix == false => {
                    number_buf.push(v);
                    self.stream.take();
                }
                // SourceChar {
                //     ch: 'a'..='z' | 'A'..='Z' | '0'..='9',
                //     ..
                // } if has_suffix => suffix_buf.push(v),
                _ => break,
            };
        }

        let suffix: Option<String> = if suffix_buf.len() > 0 {
            Some(suffix_buf.iter().map(|x| x.ch).collect::<String>())
        } else {
            None
        };
        TokenKind::Number(NumberToken {
            value: number_buf.iter().map(|x| x.ch).collect(),
            prefix: None,
            suffix,
        })
    }
}

fn strip_spacer(nr: &Vec<SourceChar>) -> Vec<SourceChar> {
    nr.iter()
        .map(|x| *x)
        .filter(|x| x.ch != '_')
        .collect::<Vec<SourceChar>>()
}

fn is_trivia(c: char) -> bool {
    match c {
        ' ' | '\t' | '\n' => true,
        _ => false,
    }
}

fn match_litteral(str: &str) -> TokenKind {
    match str {
        "let" => TokenKind::Let,
        "if" => TokenKind::If,
        // "do" => TokenKind::Do,
        "else" => TokenKind::Else,
        // "end" => TokenKind::End,
        "for" => TokenKind::For,
        "mut" => TokenKind::Mut,
        "ref" => TokenKind::Ref,
        "fn" => TokenKind::Function,
        "interface" => TokenKind::Interface,
        "trait" => TokenKind::Trait,
        "variant" => TokenKind::Variant,
        "const" => TokenKind::Const,
        "macro" => TokenKind::Macro,
        "todo" => TokenKind::Todo,
        "panic" => TokenKind::Panic,
        "self" => TokenKind::Self_,
        "mod" => TokenKind::Module,
        _ => TokenKind::Identifier(str.to_string()),
    }
}

#[cfg(test)]
mod lexer_tests {
    use super::*;
    use rstest::*;

    // lexer integrations test
    const SPC: TokenKind = TokenKind::Trivia(TokenTrivia::Space);

    fn tokenkind_vector(code: &str, skip_whitespace: bool) -> Vec<TokenKind> {
        let mut l = Lexer::new(code);
        match l.tokenize() {
            Ok(value) => value
                .iter()
                .map(|x| x.kind.clone())
                .filter(|x| if skip_whitespace { *x != SPC } else { true })
                .collect(),
            Err(_) => vec![],
        }
    }

    fn token_vector(code: &str, skip_whitespace: bool) -> Vec<Token> {
        let mut l = Lexer::new(code);
        match l.tokenize() {
            Ok(value) => value
                .iter()
                .map(|t| t.clone())
                .filter(|tok| {
                    if skip_whitespace {
                        tok.kind != SPC
                    } else {
                        true
                    }
                })
                .collect(),
            Err(_) => vec![],
        }
    }

    /// retruns a number token without prefix or suffix from griven str
    fn number_token_from_str(str: &str, suffix: Option<String>) -> TokenKind {
        TokenKind::Number(NumberToken {
            value: str.to_string(),
            prefix: None,
            suffix,
        })
    }

    fn identifier_from_str(str: &str) -> TokenKind {
        TokenKind::Identifier(str.to_string())
    }

    #[rstest]
    #[case("1.0", "1.0", None)]
    #[case("10", "10", None)]
    #[case("10_", "10", None)]
    #[case("1_000", "1000", None)]
    #[case("1.0f", "1.0", Some("f".to_string()))]
    #[case("1.0wrong", "1.0", Some("wrong".to_string()))]
    #[case(".10f", ".10", Some("f".to_string()))]
    #[case(".10", ".10", None)]
    #[case(".1_0", ".10", None)]
    #[case(".1_0_f", ".10", Some("f".to_string()))]
    fn tokenize_number(
        #[case] input: String,
        #[case] expected_value: String,
        #[case] expected_suffix: Option<String>,
    ) {
        let expected = TokenKind::Number(NumberToken {
            value: expected_value,
            prefix: None,
            suffix: expected_suffix,
        });

        let actual = tokenkind_vector(&input, true);
        assert!(actual.contains(&expected));
    }

    #[test]
    fn let_binding() {
        use TokenKind::*;

        let exp = vec![
            Let,
            Identifier("a".to_string()),
            Assign,
            Number(NumberToken {
                value: "1".to_string(),
                prefix: None,
                suffix: None,
            }),
        ];

        let res = tokenkind_vector("let a = 1", true);
        assert_eq!(exp, res);
    }

    #[test]
    fn span() {
        let tokenized = token_vector("let a = 1", true);
        let token = tokenized.get(0).unwrap();
        assert_eq!(token.span.start, (1usize, 1usize).into());
        assert_eq!(token.span.end, (1usize, 3usize).into());
        let token = tokenized.get(1usize).unwrap();
        // assert_eq!(token.span.start, (5, 6).into());
    }

    #[test]
    fn if_statement_curl() {
        use TokenKind::*;

        let left = tokenkind_vector("if a >= b { a + b }", true);
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
    fn if_else_statement() {
        use TokenKind::*;

        let left = tokenkind_vector("if a >= b { a + b } else  { a - b }", true);
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
            Else,
            OpenCurl,
            Identifier("a".to_string()),
            Minus,
            Identifier("b".to_string()),
            CloseCurl,
        ];
        assert_eq!(left, right);
    }

    #[test]
    fn range_operator() {
        use TokenKind::*;

        let left = tokenkind_vector("0..10", true);
        let right = vec![
            number_token_from_str("0", None),
            ExclusiveRange,
            number_token_from_str("10", None),
        ];
        assert_eq!(left, right);
    }

    #[test]
    fn range_inclusive_operator() {
        use TokenKind::*;

        let left = tokenkind_vector("0   ..= 10", true);
        let right = vec![
            number_token_from_str("0", None),
            InclusiveRange,
            number_token_from_str("10", None),
        ];
        assert_eq!(left, right);
    }

    #[test]
    fn attribute() {
        use TokenKind::*;

        let actual = tokenkind_vector("@test fn testFunc", true);
        let expected = vec![
            AtMarker,
            Identifier("test".to_string()),
            Function,
            Identifier("testFunc".to_string()),
        ];

        assert_eq!(actual, expected);
    }

    #[test]
    fn methods() {
        use TokenKind::*;

        let actual = tokenkind_vector(
            "fn my_struct.my_method = self, arg1 int, arg2 string -> string {}",
            true,
        );

        let expected = vec![
            Function,
            Identifier("my_struct".to_string()),
            MethodAccessor,
            Identifier("my_method".to_string()),
            Assign,
            Self_,
            Separator,
            Identifier("arg1".to_string()),
            Identifier("int".to_string()),
            Separator,
            Identifier("arg2".to_string()),
            Identifier("string".to_string()),
            Arrow,
            identifier_from_str("string"),
            OpenCurl,
            CloseCurl,
        ];

        assert_eq!(actual, expected);
    }
}
