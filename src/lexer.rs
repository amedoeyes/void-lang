#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0}:{1}: Unexpected character '{2}'")]
    UnexpectedChar(usize, usize, char),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Plus,
    Hyphen,
    Star,
    Slash,
    ParenLeft,
    ParenRight,

    Integer(String),
    Identifier(String),

    Let,

    Eof,
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

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct Lexer {
    buffer: String,
    position: Position,
}

const SYMBOLS: &[(&str, TokenKind)] = &[
    ("+", TokenKind::Plus),
    ("-", TokenKind::Hyphen),
    ("*", TokenKind::Star),
    ("/", TokenKind::Slash),
    ("(", TokenKind::ParenLeft),
    (")", TokenKind::ParenRight),
];

const KEYWORDS: &[(&str, TokenKind)] = &[("let", TokenKind::Let)];

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

    pub fn next_token(&mut self) -> Result<Token> {
        self.advance(self.slice_buffer_while(|c| c.is_whitespace()).len());

        if self.remaining_buffer().is_empty() {
            return Ok(Token {
                kind: TokenKind::Eof,
                span: Span::new(self.position, self.position),
            });
        }

        for symbol in SYMBOLS {
            if self.remaining_buffer().starts_with(symbol.0) {
                return Ok(Token {
                    kind: symbol.1.clone(),
                    span: self.advance_with_span(symbol.0.len()),
                });
            }
        }

        for keyword in KEYWORDS {
            if self.remaining_buffer().starts_with(keyword.0) {
                if let Some(next_char) = self
                    .remaining_buffer()
                    .strip_prefix(keyword.0)
                    .unwrap_or_default()
                    .chars()
                    .next()
                {
                    if !next_char.is_alphanumeric() && next_char != '_' {
                        return Ok(Token {
                            kind: keyword.1.clone(),
                            span: self.advance_with_span(keyword.0.len()),
                        });
                    }
                }
            }
        }

        let current_char = self.current_char().unwrap();

        if current_char.is_ascii_digit() {
            let slice = self.slice_buffer_while(|c| c.is_ascii_digit());
            return Ok(Token {
                kind: TokenKind::Integer(slice.to_string()),
                span: self.advance_with_span(slice.len()),
            });
        }

        if current_char.is_alphabetic() || current_char == '_' {
            let slice = self.slice_buffer_while(|c| c.is_alphanumeric() || c == '_');
            return Ok(Token {
                kind: TokenKind::Identifier(slice.to_string()),
                span: self.advance_with_span(slice.len()),
            });
        }

        Err(Error::UnexpectedChar(
            self.position.line,
            self.position.column,
            current_char,
        ))
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
        &buffer[..buffer.chars().take_while(|&c| predicate(c)).count()]
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
        let end = self.position;
        Span::new(start, end)
    }
}

pub fn tokenize(buffer: &str) -> Result<Vec<Token>> {
    let mut lexer = Lexer::new(buffer);
    let mut tokens = vec![];
    loop {
        let token = lexer.next_token()?;
        if token.kind == TokenKind::Eof {
            tokens.push(token);
            break Ok(tokens);
        }
        tokens.push(token);
    }
}
