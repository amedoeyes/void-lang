use crate::{
    lexer::{
        error::{Error, Result},
        token::{Delimiter, Keyword, Literal, Token},
    },
    span::{Position, Span},
};

#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a str,
    index: usize,
    position: Position,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            index: 0,
            position: Position { line: 1, column: 1 },
        }
    }

    pub fn next_token(&mut self) -> Result<(Token, Span)> {
        [
            Self::match_whitespaces_and_comments,
            Self::match_symbol,
            Self::match_delimiter,
            Self::match_keyword,
            Self::match_bool,
            Self::match_integer,
            Self::match_char,
            Self::match_string,
            Self::match_identifier,
            Self::match_eof,
        ]
        .iter()
        .find_map(|matcher| matcher(self))
        .unwrap_or_else(|| Err(Error::InvalidToken(Span::new(self.position, self.position))))
    }

    fn match_whitespaces_and_comments(&mut self) -> Option<Result<(Token, Span)>> {
        loop {
            match self.current_char() {
                Some(c) if c.is_whitespace() => {
                    self.advance(1);
                }
                _ if self.remaining_buffer().starts_with("//") => {
                    while let Some(c) = self.current_char() {
                        self.advance(1);
                        if c == '\n' {
                            break;
                        }
                    }
                }
                _ => break None,
            }
        }
    }

    fn match_symbol(&mut self) -> Option<Result<(Token, Span)>> {
        fn is_symbol(ch: char) -> bool {
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

        self.current_char().filter(|&c| is_symbol(c)).map(|_| {
            let (count, len) = self.scan_while(is_symbol);
            Ok((
                Token::Symbol(self.remaining_buffer()[..len].into()),
                self.advance(count),
            ))
        })
    }

    fn match_delimiter(&mut self) -> Option<Result<(Token, Span)>> {
        [
            ("(", Token::Delimiter(Delimiter::ParenLeft)),
            (")", Token::Delimiter(Delimiter::ParenRight)),
            (",", Token::Delimiter(Delimiter::Comma)),
            (";", Token::Delimiter(Delimiter::Semicolon)),
            ("[", Token::Delimiter(Delimiter::BracketLeft)),
            ("]", Token::Delimiter(Delimiter::BracketRight)),
        ]
        .iter()
        .find(|(p, _)| self.remaining_buffer().starts_with(p))
        .map(|(p, token)| Ok((token.clone(), self.advance(p.len()))))
    }

    fn match_keyword(&mut self) -> Option<Result<(Token, Span)>> {
        [
            ("op", Token::Keyword(Keyword::Op)),
            ("left", Token::Keyword(Keyword::Left)),
            ("right", Token::Keyword(Keyword::Right)),
            ("none", Token::Keyword(Keyword::None)),
            ("import", Token::Keyword(Keyword::Import)),
            ("let", Token::Keyword(Keyword::Let)),
            ("if", Token::Keyword(Keyword::If)),
            ("then", Token::Keyword(Keyword::Then)),
            ("else", Token::Keyword(Keyword::Else)),
        ]
        .iter()
        .find(|(p, _)| {
            self.remaining_buffer().starts_with(p)
                && self.remaining_buffer()[p.len()..]
                    .chars()
                    .next()
                    .is_none_or(|c| !c.is_alphanumeric() && c != '_')
        })
        .map(|(p, t)| Ok((t.clone(), self.advance(p.len()))))
    }

    fn match_bool(&mut self) -> Option<Result<(Token, Span)>> {
        [
            ("true", Token::Literal(Literal::Bool(true))),
            ("false", Token::Literal(Literal::Bool(false))),
        ]
        .iter()
        .find(|(p, _)| {
            self.remaining_buffer().starts_with(p)
                && self.remaining_buffer()[p.len()..]
                    .chars()
                    .next()
                    .is_none_or(|c| !c.is_alphanumeric() && c != '_')
        })
        .map(|(p, t)| Ok((t.clone(), self.advance(p.len()))))
    }

    fn match_integer(&mut self) -> Option<Result<(Token, Span)>> {
        self.current_char()
            .filter(|&c| c.is_ascii_digit())
            .map(|_| {
                let (count, len) = self.scan_while(|c| c.is_ascii_digit());
                Ok((
                    Token::Literal(Literal::Integer(self.remaining_buffer()[..len].into())),
                    self.advance(count),
                ))
            })
    }

    fn match_char(&mut self) -> Option<Result<(Token, Span)>> {
        self.current_char().filter(|&c| c == '\'').map(|_| {
            let (count, len) = self.scan_between('\'').ok_or(Error::Unterminated(
                Span::new(self.position, self.position),
                "\'".into(),
            ))?;

            let mut chars = self.remaining_buffer()[1..=len].chars();
            let ch = match chars.next() {
                Some('\\') => match chars.next() {
                    Some('n') => '\n',
                    Some('r') => '\r',
                    Some('t') => '\t',
                    Some('\\') => '\\',
                    Some('\'') => '\'',
                    Some('"') => '"',
                    Some('0') => '\0',
                    Some(_) => {
                        return Err(Error::InvalidEscapeChar({
                            self.advance(1);
                            self.advance(2)
                        }));
                    }
                    None => unreachable!(),
                },
                Some(c) => c,
                None => return Err(Error::EmptyChar(self.advance(2))),
            };

            if chars.next().is_some() {
                return Err(Error::InvalidChar(self.advance(count + 2)));
            }

            Ok((Token::Literal(Literal::Char(ch)), self.advance(count + 2)))
        })
    }

    fn match_string(&mut self) -> Option<Result<(Token, Span)>> {
        self.current_char().filter(|&c| c == '\"').map(|_| {
            let (count, len) = self.scan_between('\"').ok_or(Error::Unterminated(
                Span::new(self.position, self.position),
                "\"".into(),
            ))?;

            let mut str = String::new();

            let mut chars = self.remaining_buffer()[1..=len].chars().enumerate();
            while let Some((_, ch)) = chars.next() {
                if ch == '\\' {
                    str.push(match chars.next() {
                        Some((_, 'n')) => '\n',
                        Some((_, 'r')) => '\r',
                        Some((_, 't')) => '\t',
                        Some((_, '\\')) => '\\',
                        Some((_, '\'')) => '\'',
                        Some((_, '"')) => '"',
                        Some((_, '0')) => '\0',
                        Some((i, _)) => {
                            return Err(Error::InvalidEscapeChar({
                                self.advance(i);
                                self.advance(2)
                            }));
                        }
                        None => unreachable!(),
                    });
                } else {
                    str.push(ch);
                }
            }

            Ok((
                Token::Literal(Literal::String(str)),
                self.advance(count + 2),
            ))
        })
    }

    fn match_identifier(&mut self) -> Option<Result<(Token, Span)>> {
        self.current_char()
            .filter(|&c| c.is_alphabetic() || c == '_')
            .map(|_| {
                let (count, len) = self.scan_while(|c| c.is_alphanumeric() || c == '_');
                Ok((
                    Token::Identifier(self.remaining_buffer()[..len].into()),
                    self.advance(count),
                ))
            })
    }

    fn match_eof(&mut self) -> Option<Result<(Token, Span)>> {
        self.current_char().map_or_else(
            || Some(Ok((Token::Eof, Span::new(self.position, self.position)))),
            |_| None,
        )
    }

    fn current_char(&self) -> Option<char> {
        self.source[self.index..].chars().next()
    }

    fn remaining_buffer(&self) -> &str {
        &self.source[self.index..]
    }

    fn scan_between(&self, surr: char) -> Option<(usize, usize)> {
        let mut chars = self.remaining_buffer().chars();

        if chars.next()? != surr {
            return None;
        }

        let mut count = 0;
        let mut len = 0;

        loop {
            match chars.next() {
                Some(c) => match c {
                    '\n' => break None,
                    '\\' => {
                        let escaped = chars.next()?;
                        count += 2;
                        len += c.len_utf8() + escaped.len_utf8();
                    }
                    _ if c == surr => break Some((count, len)),
                    _ => {
                        count += 1;
                        len += c.len_utf8();
                    }
                },
                None => break None,
            }
        }
    }

    fn scan_while<P: Fn(char) -> bool>(&self, predicate: P) -> (usize, usize) {
        self.remaining_buffer()
            .chars()
            .take_while(|&c| predicate(c))
            .fold((0, 0), |(count, len), c| (count + 1, len + c.len_utf8()))
    }

    fn advance(&mut self, n: usize) -> Span {
        let start = self.position;

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

        let end = Position {
            column: self.position.column - 1,
            ..self.position
        };

        Span::new(start, end)
    }
}
