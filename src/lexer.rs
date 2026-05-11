use crate::diagnostics::Diagnostics;
use crate::source_char::SourceChar;
use crate::source_char::SourceIndex;
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
}

impl Lexer {
    pub fn new(code: &str) -> Self {
        let mut sourcechars: Vec<SourceChar> = Vec::with_capacity(code.len());

        //transform chars to SourceChars to get the index of every char
        let mut row = 1_usize;
        let mut col = 0_usize;

        // TODO: Remake this as an iterator
        for c in code.chars() {
            sourcechars.push(SourceChar {
                ch: c,
                index: SourceIndex { row, col },
            });
            if c == '\n' {
                row += 1;
                col = 0;
            } else {
                col += 1;
            }
        }

        // insert a \0 at the last pos so that we can peek from the last char
        // needed to get the index for the last token
        // the \0 will be tokenized as EOF
        // TODO: detta funkar men fuckar upp testen
        if let Some(last) = sourcechars.last() {
            sourcechars.push(SourceChar {
                ch: 0 as char,
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

    pub fn tokenize(&mut self, diagnostics: &mut Diagnostics) -> Result<Vec<Token>, LexerErr> {
        let mut tokens = vec![];
        let mut start = self.stream.index;

        while let Some(v) = self.stream.take() {
            let token_kind = match v {
                SourceChar { ch: '\0', .. } => TokenKind::Eof,
                // identifier, keyword
                SourceChar {
                    ch: 'a'..='z' | 'A'..='Z',
                    ..
                } => {
                    let litteral = self
                        .stream
                        .take_while_iter(SourceChar::is_alpha_or_number)
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
                    if self.stream.peek_and_step_if('=') {
                        TokenKind::Eq
                    } else if self.stream.peek_and_step_if(SourceChar::from('>')) {
                        TokenKind::FatArrow
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
                SourceChar { ch: '[', .. } => TokenKind::OpenBracket,
                SourceChar { ch: ']', .. } => TokenKind::CloseBracket,
                //string and char
                SourceChar { ch: '"', .. } => TokenKind::Error(Unexpected(v.ch)),
                SourceChar { ch: '\'', .. } => TokenKind::Error(Unexpected(v.ch)),
                //whitespace
                SourceChar { ch: '\n', .. } => TokenKind::Eol,
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

            let token = Token {
                kind: token_kind,
                span: (start, self.stream.index).into(),
            };

            // println!("{:?}", token);

            // println!("##### {:?} {:?}", start, self.stream.index);
            tokens.push(token);

            start = self.stream.index;
        }

        //the last entry will not have a correct span
        let last_index = tokens.len() - 1;
        if let Some(t) = tokens.get_mut(last_index) {
            // println!("{:#?}", t);
            t.span.end = (0_usize, last_index).into();
        }
        Ok(tokens)
    }

    /// Returs an number (int or float) from the stream and advances
    fn take_number(&mut self, sc: &SourceChar) -> TokenKind {
        let mut number_buf: Vec<SourceChar> = vec![];
        let mut suffix_buf: Vec<SourceChar> = vec![];
        let mut has_suffix = false;

        // push the first char that has already been taken by the main loop
        number_buf.push(*sc);

        while let Some(v) = self.stream.peek().copied() {
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
                } if !has_suffix => {
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

        let suffix: Option<String> = if !suffix_buf.is_empty() {
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

fn match_litteral(str: &str) -> TokenKind {
    match str {
        "let" => TokenKind::Let,
        "if" => TokenKind::If,
        "else" => TokenKind::Else,
        "for" => TokenKind::For,
        "macro" => TokenKind::Macro,
        "todo" => TokenKind::Todo,
        "panic" => TokenKind::Panic,
        "mod" => TokenKind::Module,
        "type" => TokenKind::Type,
        "opaque" => TokenKind::Opaque,
        "trait" => TokenKind::Trait,
        _ => TokenKind::Identifier(str.to_string()),
    }
}

#[cfg(test)]
mod lexer_tests {
    use super::*;
    use crate::{span::Span, test_utils::SnapshotStr};
    use rstest::*;

    // lexer integrations test
    const SPC: TokenKind = TokenKind::Trivia(TokenTrivia::Space);
    const TAB: TokenKind = TokenKind::Trivia(TokenTrivia::Tab);

    fn token_vector(code: &str, skip_whitespace: bool) -> Vec<Token> {
        let mut diag = Diagnostics::new();
        let mut l = Lexer::new(code);
        match l.tokenize(&mut diag) {
            Ok(value) => value
                .iter()
                .filter(|&tok| {
                    if skip_whitespace {
                        tok.kind != SPC && tok.kind != TAB
                    } else {
                        true
                    }
                })
                .cloned()
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
    #[case("1.0", "1.0", "", "")]
    #[case("10", "10", "", "")]
    #[case("10_", "10", "", "")]
    #[case("1_000", "1000", "", "")]
    #[case("1.0f", "1.0", "", "f")]
    #[case("1.0wrong", "1.0", "", "wrong")]
    #[case(".10f", ".10", ".", "f")]
    #[case(".10", ".10", ".", "")]
    #[case(".1_0", ".10", ".", "")]
    #[case(".1_0_f", ".10", ".", "f")]
    fn tokenize_number(
        #[case] input: String,
        #[case] expected_value: String,
        #[case] expected_prefix: &str,
        #[case] expected_suffix: &str,
    ) {
        let expected = TokenKind::Number(NumberToken {
            value: expected_value,
            prefix: if expected_prefix.is_empty() {
                None
            } else {
                Some(expected_prefix.to_string())
            },
            suffix: if expected_suffix.is_empty() {
                None
            } else {
                Some(expected_suffix.to_string())
            },
        });

        let actual = token_vector(&input, true).first().map(|t| &t.kind).cloned();
        assert_eq!(actual, Some(expected));
    }

    #[rstest]
    #[case(0, (1,0), (1,3))]
    #[case(1, (1,4), (1,5))]
    #[case(5, (2,0), (2,3))]
    fn span(
        #[case] index: usize,
        #[case] expected_start: (i32, i32),
        #[case] expected_end: (i32, i32),
    ) {
        let tokenized = token_vector("let a = 1\nlet b = 2", true);
        let token = tokenized.get(index).unwrap();
        assert_eq!(token.span.start, SourceIndex::from(expected_start));
        assert_eq!(token.span.end, SourceIndex::from(expected_end));
    }

    #[test]
    fn tokenize_let_binding_const() {
        insta::assert_snapshot!(token_vector("let a = 2", false).snapshot());
    }

    #[test]
    fn tokenize_let_binding_expr() {
        let tokens = token_vector("let a = 1 + 2", true);
        insta::assert_snapshot!(tokens.snapshot());
    }

    #[test]
    fn tokenize_let_binding_func() {
        let tokens = token_vector("let my_fn = { a, b => a + b }", true);
        insta::assert_snapshot!(tokens.snapshot());
    }

    #[test]
    fn tokenize_type_decl_record() {
        let tokens = token_vector("type MyType = { a Int, b [Gt, Lt] }", true);
        insta::assert_snapshot!(tokens.snapshot());
    }

    #[test]
    fn tokenize_new_line() {
        let tokenized = token_vector("\n", false);
        let actual = tokenized.first().unwrap();
        assert_eq!(TokenKind::Eol, actual.kind);
    }

    #[test]
    fn if_else_expr() {
        let tokens = token_vector("if a >= b { a + b } else  { a - b }", true);
        insta::assert_snapshot!(tokens.snapshot())
    }

    #[test]
    fn range_operator() {
        insta::assert_snapshot!(token_vector("0..10", true).snapshot());
    }

    #[test]
    fn range_inclusive_operator() {
        insta::assert_snapshot!(token_vector("0..=10", true).snapshot());
    }
}
