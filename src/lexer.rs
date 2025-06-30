use core::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Plus,
    Hyphen,
    Star,
    Slash,
    ParenLeft,
    ParenRight,
    HyphenGreaterThan,

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

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Hyphen => write!(f, "-"),
            TokenKind::Star => write!(f, "*"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::ParenLeft => write!(f, "("),
            TokenKind::ParenRight => write!(f, ")"),
            TokenKind::HyphenGreaterThan => write!(f, "->"),

            TokenKind::Boolean(val) => write!(f, "{val}"),
            TokenKind::Integer(val) => write!(f, "{val}"),
            TokenKind::Identifier(val) => write!(f, "{val}"),

            TokenKind::Let => write!(f, "let"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Then => write!(f, "then"),
            TokenKind::Else => write!(f, "else"),

            TokenKind::Eof => write!(f, "EOF"),

            TokenKind::Invalid => write!(f, "invalid token"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Position {
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}", self.start.line, self.start.column)
    }
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn merge(&mut self, other: &Self) -> &Self {
        self.end = other.end;
        self
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

const SYMBOLS: &[(&str, TokenKind)] = &[
    ("->", TokenKind::HyphenGreaterThan),
    ("+", TokenKind::Plus),
    ("-", TokenKind::Hyphen),
    ("*", TokenKind::Star),
    ("/", TokenKind::Slash),
    ("(", TokenKind::ParenLeft),
    (")", TokenKind::ParenRight),
];

const KEYWORDS: &[(&str, TokenKind)] = &[
    ("let", TokenKind::Let),
    ("if", TokenKind::If),
    ("then", TokenKind::Then),
    ("else", TokenKind::Else),
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

    pub fn next_token(&mut self) -> Token {
        self.advance(self.slice_buffer_while(|c| c.is_whitespace()).len());

        while self.remaining_buffer().starts_with("//") {
            self.advance(self.slice_buffer_while(|c| c != '\n').len());
            self.advance(self.slice_buffer_while(|c| c.is_whitespace()).len());
        }

        if self.remaining_buffer().is_empty() {
            return Token {
                kind: TokenKind::Eof,
                span: Span::new(self.position, self.position),
            };
        }

        for symbol in SYMBOLS {
            if self.remaining_buffer().starts_with(symbol.0) {
                return Token {
                    kind: symbol.1.clone(),
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
                return Token {
                    kind: keyword.1.clone(),
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
                    return Token {
                        kind: TokenKind::Boolean(pattern.to_string()),
                        span: self.advance_with_span(pattern.len()),
                    };
                }
            }
        }

        if current_char.is_ascii_digit() {
            let slice = self.slice_buffer_while(|c| c.is_ascii_digit());
            return Token {
                kind: TokenKind::Integer(slice.to_string()),
                span: self.advance_with_span(slice.len()),
            };
        }

        if current_char.is_alphabetic() || current_char == '_' {
            let slice = self.slice_buffer_while(|c| c.is_alphanumeric() || c == '_');
            return Token {
                kind: TokenKind::Identifier(slice.to_string()),
                span: self.advance_with_span(slice.len()),
            };
        }

        Token {
            kind: TokenKind::Invalid,
            span: Span::new(self.position, self.position),
        }
    }

    fn current_char(&self) -> Option<char> {
        self.buffer.chars().nth(self.position.index)
    }

    fn remaining_buffer(&self) -> &str {
        &self.buffer[self.position.index..]
    }

    fn slice_buffer(&self, span: &Span) -> &str {
        &self.buffer[span.start.index..span.end.index]
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
