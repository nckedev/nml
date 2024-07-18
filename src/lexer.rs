use core::panic;
use std::usize;

use crate::source_char::SourceChar;
use crate::source_char::SourceIndex;
use crate::stream::Stream;
use crate::token::NumberToken;
use crate::token::Token;
use crate::token::TokenError::Unexpected;
use crate::token::TokenKind;
use crate::token::TokenSpan;
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
                    if self
                        .stream
                        .peek_and_step_if_fn(|x| SourceChar::test(x, 's'))
                    {
                        //if self.stream.peek_and_step_if(SourceChar::from('=')) {
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
                SourceChar { ch: '-', .. } => TokenKind::Minus,
                SourceChar { ch: '+', .. } => TokenKind::Plus,
                SourceChar { ch: '*', .. } => TokenKind::Mul,
                SourceChar { ch: '/', .. } => TokenKind::Div,

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
                SourceChar { ch: '{', .. } => TokenKind::OpenCurl,
                SourceChar { ch: '}', .. } => TokenKind::CloseCurl,
                SourceChar { ch: '(', .. } => TokenKind::OpenParen,
                SourceChar { ch: ')', .. } => TokenKind::CloseParen,
                //string and char
                SourceChar { ch: '"', .. } => TokenKind::Error(Unexpected),
                SourceChar { ch: '\'', .. } => TokenKind::Error(Unexpected),
                //whitespace
                SourceChar { ch: '\n', .. } => TokenKind::Trivia(TokenTrivia::EOL),
                SourceChar { ch: '\t', .. } => TokenKind::Trivia(TokenTrivia::Tab),
                SourceChar { ch: ' ', .. } => TokenKind::Trivia(TokenTrivia::Space),

                //attribute, @test
                SourceChar { ch: '@', .. } => {
                    //take until space or newline
                    //Todo: attribute with parameters @test(arg1, arg2)
                    let attr_arr = self.stream.take_until(|x| x.ch == ' ' || x.ch == '\n');
                    let str: String = attr_arr.into_iter().map(|x| x.ch).collect();
                    TokenKind::Attribute(str)
                }
                _ => TokenKind::Error(Unexpected),
            };

            let end_index = if let Some(t) = self.stream.peek() {
                t.index
            } else {
                SourceIndex { row: 0, col: 0 }
            };

            let token = Token {
                kind: token_kind,
                span: TokenSpan {
                    start: v.index,
                    end: end_index,
                },
            };
            tokens.push(token);
        }
        Ok(tokens)
    }
    /// Returs an number (int or float) from the stream and advances
    ///
    /// cases
    /// ```
    /// 1..3 => range
    /// 1. => float
    /// 1f => float
    /// 1.f => float
    /// 1 => int
    /// 1i => int
    /// 1u => uint
    /// ```
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
        "do" => TokenKind::Do,
        "else" => TokenKind::Else,
        "end" => TokenKind::End,
        "for" => TokenKind::For,
        "mut" => TokenKind::Mut,
        "ref" => TokenKind::Ref,
        "interface" => TokenKind::Interface,
        "trait" => TokenKind::Trait,
        "variant" => TokenKind::Variant,
        "const" => TokenKind::Const,
        _ => TokenKind::Identifier(str.to_string()),
    }
}

#[cfg(test)]
mod lexer_tests {
    use super::*;
    use rstest::*;

    // lexer integrations test
    const SPC: TokenKind = TokenKind::Trivia(TokenTrivia::Space);

    fn tokenize_filter_whitespace(code: &str, skip_whitespace: bool) -> Vec<TokenKind> {
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

    /// retruns a number token without prefix or suffix from griven str
    fn number_token_from_str(str: &str, suffix: Option<String>) -> TokenKind {
        TokenKind::Number(NumberToken {
            value: str.to_string(),
            prefix: None,
            suffix,
        })
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

        let actual = tokenize_filter_whitespace(&input, true);
        assert!(actual.contains(&expected));
    }

    #[test]
    fn lexer_parse_let_binding() {
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

        let res = tokenize_filter_whitespace("let a = 1", true);
        assert_eq!(exp, res);
    }

    #[test]
    fn lexer_parse_if_statement() {
        use TokenKind::*;

        let left = tokenize_filter_whitespace("if a >= b do a + b end", true);
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
        use TokenKind::*;

        let left = tokenize_filter_whitespace("if a >= b { a + b }", true);
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
        use TokenKind::*;

        let left = tokenize_filter_whitespace("if a >= b do a + b else a - b end", false);
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
        use TokenKind::*;

        let left = tokenize_filter_whitespace("0..10", false);
        let right = vec![
            number_token_from_str("0", None),
            ExclusiveRange,
            number_token_from_str("10", None),
        ];
        assert_eq!(left, right);
    }

    #[test]
    fn lexer_parse_range_inclusive_operator() {
        use TokenKind::*;

        let left = tokenize_filter_whitespace("0..=10", false);
        let right = vec![
            number_token_from_str("0", None),
            InclusiveRange,
            number_token_from_str("10", None),
        ];
        assert_eq!(left, right);
    }
}
