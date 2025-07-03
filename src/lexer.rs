use core::fmt;

use crate::span::{Position, Span, Spanned};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Equal,
    EqualEqual,
    Hyphen,
    HyphenGreaterThan,
    ParenLeft,
    ParenRight,
    Percent,
    Plus,
    Semicolon,
    Slash,
    Star,

    Boolean(String),
    Integer(String),
    Identifier(String),

    Let,
    If,
    Then,
    Else,

    Eof,

    Invalid,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Token::Equal => write!(f, "="),
            Token::EqualEqual => write!(f, "=="),
            Token::Hyphen => write!(f, "-"),
            Token::HyphenGreaterThan => write!(f, "->"),
            Token::ParenLeft => write!(f, "("),
            Token::ParenRight => write!(f, ")"),
            Token::Percent => write!(f, "%"),
            Token::Plus => write!(f, "+"),
            Token::Semicolon => write!(f, ";"),
            Token::Slash => write!(f, "/"),
            Token::Star => write!(f, "*"),

            Token::Boolean(val) => write!(f, "{val}"),
            Token::Integer(val) => write!(f, "{val}"),
            Token::Identifier(val) => write!(f, "{val}"),

            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),

            Token::Eof => write!(f, "EOF"),

            Token::Invalid => write!(f, "invalid token"),
        }
    }
}

const SYMBOLS: &[(&str, Token)] = &[
    ("->", Token::HyphenGreaterThan),
    ("==", Token::EqualEqual),
    ("%", Token::Percent),
    ("(", Token::ParenLeft),
    (")", Token::ParenRight),
    ("*", Token::Star),
    ("+", Token::Plus),
    ("-", Token::Hyphen),
    ("/", Token::Slash),
    (";", Token::Semicolon),
    ("=", Token::Equal),
];

const KEYWORDS: &[(&str, Token)] = &[
    ("let", Token::Let),
    ("if", Token::If),
    ("then", Token::Then),
    ("else", Token::Else),
];

#[derive(Debug)]
pub struct Lexer {
    buffer: String,
    position: Position,
}

impl Lexer {
    pub fn new(buffer: &str) -> Self {
        Self {
            buffer: buffer.to_string(),
            position: Position {
                index: 0,
                line: 1,
                column: 1,
            },
        }
    }

    pub fn next_token(&mut self) -> Spanned<Token> {
        self.advance(self.slice_buffer_while(|c| c.is_whitespace()).len());

        while self.remaining_buffer().starts_with("//") {
            self.advance(self.slice_buffer_while(|c| c != '\n').len());
            self.advance(self.slice_buffer_while(|c| c.is_whitespace()).len());
        }

        if self.remaining_buffer().is_empty() {
            return Spanned {
                value: Token::Eof,
                span: Span::new(self.position, self.position),
            };
        }

        for symbol in SYMBOLS {
            if self.remaining_buffer().starts_with(symbol.0) {
                return Spanned {
                    value: symbol.1.clone(),
                    span: self.advance_with_span(symbol.0.len()),
                };
            }
        }

        for keyword in KEYWORDS {
            if self.remaining_buffer().starts_with(keyword.0)
                && let Some(next_char) = self
                    .remaining_buffer()
                    .strip_prefix(keyword.0)
                    .unwrap_or_default()
                    .chars()
                    .next()
                && !next_char.is_alphanumeric()
                && next_char != '_'
            {
                return Spanned {
                    value: keyword.1.clone(),
                    span: self.advance_with_span(keyword.0.len()),
                };
            }
        }

        let current_char = self.current_char().unwrap();

        if current_char == 't' || current_char == 'f' {
            for pattern in ["true", "false"] {
                if self.remaining_buffer().starts_with(pattern)
                    && let Some(next_char) = self
                        .remaining_buffer()
                        .strip_prefix(pattern)
                        .unwrap_or_default()
                        .chars()
                        .next()
                    && !next_char.is_alphanumeric()
                {
                    return Spanned {
                        value: Token::Boolean(pattern.to_string()),
                        span: self.advance_with_span(pattern.len()),
                    };
                }
            }
        }

        if current_char.is_ascii_digit() {
            let slice = self.slice_buffer_while(|c| c.is_ascii_digit());
            return Spanned {
                value: Token::Integer(slice.to_string()),
                span: self.advance_with_span(slice.len()),
            };
        }

        if current_char.is_alphabetic() || current_char == '_' {
            let slice = self.slice_buffer_while(|c| c.is_alphanumeric() || c == '_');
            return Spanned {
                value: Token::Identifier(slice.to_string()),
                span: self.advance_with_span(slice.len()),
            };
        }

        Spanned {
            value: Token::Invalid,
            span: Span::new(self.position, self.position),
        }
    }

    fn current_char(&self) -> Option<char> {
        self.buffer.chars().nth(self.position.index)
    }

    fn remaining_buffer(&self) -> &str {
        &self.buffer[self.position.index..]
    }

    fn slice_buffer_while<P: Fn(char) -> bool>(&self, predicate: P) -> &str {
        let buffer = self.remaining_buffer();
        if let Some(pos) = buffer.find(|c| !predicate(c)) {
            &buffer[..pos]
        } else {
            buffer
        }
    }

    fn advance(&mut self, n: usize) {
        for _ in 0..n {
            if let Some(c) = self.current_char() {
                if c == '\n' {
                    self.position.line += 1;
                    self.position.column = 1;
                } else {
                    self.position.column += 1;
                }
                self.position.index += c.len_utf8();
            }
        }
    }

    fn advance_with_span(&mut self, n: usize) -> Span {
        let start = self.position;
        self.advance(n);
        let mut end = self.position;
        end.index -= 1;
        end.column -= 1;
        Span::new(start, end)
    }
}
