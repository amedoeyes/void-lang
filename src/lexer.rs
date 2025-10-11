use core::fmt;

use crate::{
    error::SyntaxError,
    span::{Position, Span},
};

type Result<T> = std::result::Result<T, SyntaxError>;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    BracketLeft,
    BracketRight,
    Comma,
    Equal,
    ParenLeft,
    ParenRight,
    Semicolon,
    HyphenGreaterThan,

    Boolean(String),
    Integer(String),
    Char(char),
    String(String),
    Identifier(String),

    Let,
    If,
    Then,
    Else,

    Operator(String),

    Eof,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Token::BracketLeft => write!(f, "["),
            Token::BracketRight => write!(f, "]"),
            Token::Comma => write!(f, ","),
            Token::Equal => write!(f, "="),
            Token::HyphenGreaterThan => write!(f, "->"),
            Token::ParenLeft => write!(f, "("),
            Token::ParenRight => write!(f, ")"),
            Token::Semicolon => write!(f, ";"),
            Token::Boolean(val) => write!(f, "{val}"),
            Token::Integer(val) => write!(f, "{val}"),
            Token::Char(val) => write!(f, "'{val}'"),
            Token::String(val) => write!(f, "\"{val}\""),
            Token::Identifier(val) => write!(f, "{val}"),
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::Operator(op) => write!(f, "({op})"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

const SYMBOLS: &[(&str, Token)] = &[
    ("->", Token::HyphenGreaterThan),
    ("(", Token::ParenLeft),
    (")", Token::ParenRight),
    (",", Token::Comma),
    (";", Token::Semicolon),
    ("=", Token::Equal),
    ("[", Token::BracketLeft),
    ("]", Token::BracketRight),
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
    index: usize,
    position: Position,
}

impl Lexer {
    pub fn new(buffer: &str) -> Self {
        Self {
            buffer: buffer.to_string(),
            index: 0,
            position: Position { line: 1, column: 1 },
        }
    }

    pub fn next_token(&mut self) -> Result<(Token, Span)> {
        self.advance(self.slice_buffer_while(|c| c.is_whitespace()).len());

        while self.remaining_buffer().starts_with("//") {
            self.advance(self.slice_buffer_while(|c| c != '\n').len());
            self.advance(self.slice_buffer_while(|c| c.is_whitespace()).len());
        }

        if self.remaining_buffer().is_empty() {
            return Ok((Token::Eof, Span::new(self.position, self.position)));
        }

        let current_char = self.current_char().unwrap();

        if is_operator_char(current_char) {
            let slice = self.slice_buffer_while(is_operator_char);
            if !SYMBOLS.iter().any(|(sym, _)| slice == *sym) {
                return Ok((
                    Token::Operator(slice.to_string()),
                    self.advance_with_span(slice.len()),
                ));
            }
        }

        for symbol in SYMBOLS {
            if self.remaining_buffer().starts_with(symbol.0) {
                return Ok((symbol.1.clone(), self.advance_with_span(symbol.0.len())));
            }
        }

        for keyword in KEYWORDS {
            if self.remaining_buffer().starts_with(keyword.0)
                && self
                    .remaining_buffer()
                    .strip_prefix(keyword.0)
                    .unwrap_or_default()
                    .chars()
                    .next()
                    .is_none_or(|c| !c.is_alphanumeric() && c != '_')
            {
                return Ok((keyword.1.clone(), self.advance_with_span(keyword.0.len())));
            }
        }

        if current_char == 't' || current_char == 'f' {
            for pattern in ["true", "false"] {
                if self.remaining_buffer().starts_with(pattern)
                    && self
                        .remaining_buffer()
                        .strip_prefix(pattern)
                        .unwrap_or_default()
                        .chars()
                        .next()
                        .is_none_or(|c| !c.is_alphanumeric())
                {
                    return Ok((
                        Token::Boolean(pattern.to_string()),
                        self.advance_with_span(pattern.len()),
                    ));
                }
            }
        }

        if current_char.is_ascii_digit() {
            let slice = self.slice_buffer_while(|c| c.is_ascii_digit());
            return Ok((
                Token::Integer(slice.to_string()),
                self.advance_with_span(slice.len()),
            ));
        }

        if current_char.is_alphabetic() || current_char == '_' {
            let slice = self.slice_buffer_while(|c| c.is_alphanumeric() || c == '_');
            return Ok((
                Token::Identifier(slice.to_string()),
                self.advance_with_span(slice.len()),
            ));
        }

        if current_char == '\'' {
            let slice =
                self.slice_buffer_between('\'', false)
                    .ok_or(SyntaxError::UnterminatedChar(Span::new(
                        self.position,
                        self.position,
                    )))?;
            let mut chars = slice.chars();

            let ch = match chars.next() {
                Some('\\') => match chars.next() {
                    Some('n') => '\n',
                    Some('r') => '\r',
                    Some('t') => '\t',
                    Some('\\') => '\\',
                    Some('\'') => '\'',
                    Some('"') => '"',
                    Some('0') => '\0',
                    _ => {
                        return Err(SyntaxError::InvalidEscapeChar(self.span_from_range(1, 2)));
                    }
                },
                Some(char) => char,
                None => {
                    return Err(SyntaxError::EmptyChar(self.span_from_range(0, 1)));
                }
            };

            if chars.next().is_some() {
                return Err(SyntaxError::InvalidChar(
                    self.span_from_range(0, slice.len() + 1),
                ));
            }

            return Ok((Token::Char(ch), self.advance_with_span(slice.len() + 2)));
        }

        if current_char == '\"' {
            let slice =
                self.slice_buffer_between('\"', false)
                    .ok_or(SyntaxError::UnterminatedString(Span::new(
                        self.position,
                        self.position,
                    )))?;
            let mut str = String::new();

            let mut chars = slice.chars();
            while let Some(ch) = chars.next() {
                if ch == '\\' {
                    str.push(match chars.next() {
                        Some('n') => '\n',
                        Some('r') => '\r',
                        Some('t') => '\t',
                        Some('\\') => '\\',
                        Some('\'') => '\'',
                        Some('"') => '"',
                        Some('0') => '\0',
                        _ => {
                            return Err(SyntaxError::InvalidEscapeChar(
                                self.span_from_range(str.len() + 1, str.len() + 2),
                            ));
                        }
                    });
                } else {
                    str.push(ch);
                }
            }
            return Ok((
                Token::String(slice.to_string()),
                self.advance_with_span(slice.len() + 2),
            ));
        }

        Err(SyntaxError::InvalidToken(Span::new(
            self.position,
            self.position,
        )))
    }

    fn current_char(&self) -> Option<char> {
        self.buffer.chars().nth(self.index)
    }

    fn remaining_buffer(&self) -> &str {
        &self.buffer[self.index..]
    }

    fn slice_buffer_between(&self, surr: char, include_newlines: bool) -> Option<&str> {
        let buffer = self.remaining_buffer();

        if !buffer.starts_with(surr) {
            return None;
        }

        let end = buffer
            .char_indices()
            .skip(1)
            .take_while(|&(_, c)| include_newlines || c != '\n')
            .find(|&(_, c)| c == surr)?
            .0;

        Some(&buffer[1..end])
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
                self.index += c.len_utf8();
            }
        }
    }

    fn span_from_range(&self, start: usize, end: usize) -> Span {
        Span::new(self.position_after(start), self.position_after(end))
    }

    fn position_after(&self, n: usize) -> Position {
        let mut pos = self.position;
        for _ in 0..n {
            if let Some(c) = self.current_char() {
                if c == '\n' {
                    pos.line += 1;
                    pos.column = 1;
                } else {
                    pos.column += 1;
                }
            }
        }
        pos
    }

    fn advance_with_span(&mut self, n: usize) -> Span {
        let start = self.position;
        self.advance(n);
        let mut end = self.position;
        end.column -= 1;
        Span::new(start, end)
    }
}

fn is_operator_char(ch: char) -> bool {
    matches!(
        ch,
        '!' | '#'
            | '$'
            | '%'
            | '&'
            | '*'
            | '+'
            | '-'
            | '.'
            | '/'
            | ':'
            | '<'
            | '='
            | '>'
            | '?'
            | '@'
            | '\\'
            | '^'
            | '|'
            | '~'
    )
}
