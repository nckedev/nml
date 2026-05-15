use crate::diagnostics::Diagnostics;
use crate::parser::EndOfStream;
use crate::parser::ParseErr;
use crate::source_char::SourCharIterTrait;
use crate::source_char::SourceChar;
use crate::source_char::SourceCharIter;
use crate::source_char::SourceIndex;
use crate::stream::Stream;
use crate::token::NumberToken;
use crate::token::NumberTokenPrefix;
use crate::token::NumberTokenSuffix;
use crate::token::Token;
use crate::token::TokenError::Unexpected;
use crate::token::TokenKind;
use crate::token::TokenTrivia;

#[derive(Debug)]
pub struct LexerErr {}

pub fn tokenize(code: &str, _diagnostics: &mut Diagnostics) -> Result<Vec<Token>, LexerErr> {
    let mut stream = Stream::new(code.source_chars());
    let mut tokens = vec![];

    while let Some(v) = stream.take() {
        let token_kind = match v {
            SourceChar { ch: '\0', .. } => TokenKind::Eof,
            // identifier, keyword
            SourceChar {
                ch: 'a'..='z' | 'A'..='Z',
                ..
            } => {
                let litteral = stream
                    .take_while_iter(SourceChar::is_alpha_or_number)
                    .map(|x| x.ch)
                    .collect::<String>();

                match_litteral(&(v.ch.to_string() + &litteral))
            }
            //number
            SourceChar {
                ch: '0'..='9',
                index: _start,
                ..
            } => take_number(&mut stream, &v),
            //discard _
            SourceChar { ch: '_', .. } => TokenKind::Discard,
            // = or == or =>
            SourceChar { ch: '=', .. } => {
                if stream.peek_and_step_if('=') {
                    TokenKind::Eq
                } else if stream.peek_and_step_if(SourceChar::from('>')) {
                    TokenKind::FatArrow
                } else {
                    TokenKind::Assign
                }
            }
            // >= or >
            SourceChar { ch: '>', .. } => match stream.peek_and_step_if(SourceChar::from('=')) {
                true => TokenKind::GtEq,
                false => TokenKind::Gt,
            },
            // <= or <
            SourceChar { ch: '<', .. } => match stream.peek_and_step_if(SourceChar::from('=')) {
                true => TokenKind::LtEq,
                false => TokenKind::Lt,
            },
            //arrow or minus
            SourceChar { ch: '-', .. } => {
                if stream.peek_and_step_if(SourceChar::from('>')) {
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
                if stream.take_if_fn(|c| c.ch == '.').is_some() {
                    if stream.peek_and_step_if(SourceChar::from('=')) {
                        // inclusive rage ..=
                        TokenKind::InclusiveRange
                    } else {
                        // range ..
                        TokenKind::ExclusiveRange
                    }
                } else {
                    // method accessor, or whatever its called
                    TokenKind::MethodAccessor
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
                if stream.peek_expect(SourceChar::is_alpha) {
                    TokenKind::AtMarker
                } else {
                    TokenKind::Error(Unexpected(v.ch))
                }

                // //take until space or newline
                // //Todo: attribute with parameters @test(arg1, arg2)
                // let attr_arr = stream.take_until(|x| x.ch == ' ' || x.ch == '\n');
                // let str: String = attr_arr.into_iter().map(|x| x.ch).collect();
                // TokenKind::Attribute(str)
            }

            //np match
            _ => TokenKind::Error(Unexpected(v.ch)),
        };

        // get the index of the next token and use that as the end
        let end = if let Some(next) = stream.peek() {
            next.index
        } else {
            SourceIndex::default()
        };

        let token = Token {
            kind: token_kind,
            span: (v.index, end).into(),
        };

        tokens.push(token);
    } // end of while

    //the last entry will not have a correct span
    let last_index = tokens.len() - 1;
    if let Some(t) = tokens.get_mut(last_index) {
        // println!("{:#?}", t);
        t.span.end = (0_usize, last_index).into();
    }
    Ok(tokens)
}

/// Returs an number (int or float) from the stream and advances
fn take_number(stream: &mut Stream<SourceChar, SourceCharIter<'_>>, sc: &SourceChar) -> TokenKind {
    let mut number_buf: Vec<SourceChar> = vec![];
    let mut has_dot = false;
    let mut suffix = NumberTokenSuffix::None;

    // push the first char that has already been taken by the main loop
    let prefix = match sc.ch {
        '0' if let Some(SourceChar { ch, .. }) = stream.peek() => match ch {
            'x' => {
                let _ = stream.take();
                NumberTokenPrefix::Hex
            }
            'b' => {
                let _ = stream.take();
                NumberTokenPrefix::Bin
            }
            'o' => {
                let _ = stream.take();
                NumberTokenPrefix::Oct
            }
            '.' => {
                // 0..  this is a range operator, return just the 0
                if stream.peek_n_expect(1, |c| c.ch == '.') {
                    return TokenKind::Number(NumberToken {
                        value: "0".to_string(),
                        prefix: NumberTokenPrefix::None,
                        suffix: NumberTokenSuffix::None,
                    });
                } else {
                    number_buf.push(*sc);
                    NumberTokenPrefix::None
                }
            }
            '0'..='9' | 'A'..='F' => {
                number_buf.push(*sc);
                NumberTokenPrefix::None
            }
            x => NumberTokenPrefix::Invalid(*x),
        },
        _ => {
            number_buf.push(*sc);
            NumberTokenPrefix::None
        }
    };

    while let Some(v) = stream.peek().copied() {
        match v {
            SourceChar { ch: 'f', .. }
                if prefix == NumberTokenPrefix::None
                    && stream.peek_n_expect(1, |c| !SourceChar::is_alpha_or_number(c)) =>
            {
                stream.take();
                suffix = NumberTokenSuffix::Float;
                break;
            }
            SourceChar {
                ch: 'a'..='f' | 'A'..='F',
                ..
            } => {
                number_buf.push(v);
                let _ = stream.take();
            }
            SourceChar { ch: '.', .. } if !has_dot => {
                // if there is two dots in a row it is a range operator
                // so return what we have got so far as a IntNumber
                if stream.peek_n_expect(1, |x| x.ch == '.') {
                    break;
                }
                number_buf.push(v);
                stream.take();
                has_dot = true;
            }
            SourceChar {
                ch: '_', index: _, ..
            } => {
                stream.take();
            }
            SourceChar {
                ch: '0'..='9',
                index: _,
                ..
            } => {
                number_buf.push(v);
                stream.take();
            }
            // SourceChar {
            //     ch: 'a'..='z' | 'A'..='Z' | '0'..='9',
            //     ..
            // } if has_suffix => suffix_buf.push(v),
            _ => break,
        };
    }

    TokenKind::Number(NumberToken {
        value: number_buf.iter().map(|x| x.ch).collect(),
        prefix,
        suffix,
    })
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
    use crate::{
        lexer,
        test_utils::SnapshotStr,
        token::{NumberTokenPrefix, NumberTokenSuffix},
    };
    use rstest::*;

    // lexer integrations test
    const SPC: TokenKind = TokenKind::Trivia(TokenTrivia::Space);
    const TAB: TokenKind = TokenKind::Trivia(TokenTrivia::Tab);

    fn token_vector(code: &str, skip_whitespace: bool) -> Vec<Token> {
        let mut diag = Diagnostics::new();
        match lexer::tokenize(code, &mut diag) {
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

    // /// retruns a number token without prefix or suffix from griven str
    // fn number_token_from_str(str: &str, suffix: Option<String>) -> TokenKind {
    //     TokenKind::Number(NumberToken {
    //         value: str.to_string(),
    //         prefix: None,
    //         suffix,
    //     })
    // }

    #[rstest]
    #[case("1.0", "1.0", NumberTokenPrefix::None, NumberTokenSuffix::None)]
    #[case("10", "10", NumberTokenPrefix::None, NumberTokenSuffix::None)]
    #[case("10_", "10", NumberTokenPrefix::None, NumberTokenSuffix::None)]
    #[case("1_000", "1000", NumberTokenPrefix::None, NumberTokenSuffix::None)]
    #[case("1.0f", "1.0", NumberTokenPrefix::None, NumberTokenSuffix::Float)]
    #[case("10f", "10", NumberTokenPrefix::None, NumberTokenSuffix::Float)]
    #[case("1_0_f", "10", NumberTokenPrefix::None, NumberTokenSuffix::Float)]
    #[case("2.23", "2.23", NumberTokenPrefix::None, NumberTokenSuffix::None)]
    #[case("0xFF", "FF", NumberTokenPrefix::Hex, NumberTokenSuffix::None)]
    fn tokenize_number(
        #[case] input: String,
        #[case] expected_value: String,
        #[case] expected_prefix: NumberTokenPrefix,
        #[case] expected_suffix: NumberTokenSuffix,
    ) {
        let expected = TokenKind::Number(NumberToken {
            value: expected_value,
            prefix: expected_prefix,
            suffix: expected_suffix,
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
