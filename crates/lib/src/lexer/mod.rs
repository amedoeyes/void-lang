use crate::span::{Position, Span};
pub use error::{Error, Result};
pub use token::{Delimiter, Keyword, Literal, Token};

mod error;
mod token;

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
            Self::match_integer,
            Self::match_char,
            Self::match_string,
            Self::match_type,
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
            ("{", Token::Delimiter(Delimiter::BraceLeft)),
            ("}", Token::Delimiter(Delimiter::BraceRight)),
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
            ("primitive", Token::Keyword(Keyword::Primitive)),
            ("type", Token::Keyword(Keyword::Type)),
            ("enum", Token::Keyword(Keyword::Enum)),
            ("let", Token::Keyword(Keyword::Let)),
            ("match", Token::Keyword(Keyword::Match)),
            ("with", Token::Keyword(Keyword::With)),
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

    fn match_type(&mut self) -> Option<Result<(Token, Span)>> {
        self.current_char()
            .filter(|&c| c.is_alphabetic() && c.is_uppercase())
            .map(|_| {
                let (count, len) = self.scan_while(|c| c.is_alphanumeric() || c == '_');
                Ok((
                    Token::Type(self.remaining_buffer()[..len].into()),
                    self.advance(count),
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

pub fn is_symbol(ch: char) -> bool {
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

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    macro_rules! assert_tokens {
        ($source:expr, { $( $l1:literal : $c1:literal - $l2:literal : $c2:literal : $t:ident $( ( $($a:tt)* ) )? )* } $(,)?) => {{
            let tokens = tokenize($source);
            assert_eq!(
                tokens.is_ok(),
                true,
                "Expected successful tokenization but got error: {:?}",
                tokens.unwrap_err()
            );
            assert_eq!(tokens.unwrap(), &[ $( (Span::from((($l1, $c1), ($l2, $c2))), assert_tokens!(@token $t $( $($a)* )?)) ),* ]);
        }};

        (@token Symbol $($sym:tt)*) => { Token::Symbol(stringify!($($sym)*).into()) };
        (@token Delimiter $delim:ident) => { Token::Delimiter(Delimiter::$delim) };
        (@token Keyword $kw:ident) => { Token::Keyword(Keyword::$kw) };
        (@token Literal $lit:ident ( $($val:tt)* )) => { assert_tokens!(@literal $lit ( $($val)* )) };
        (@token Identifier $id:expr) => { Token::Identifier($id.into()) };
        (@token Eof) => { Token::Eof };

        (@literal Integer ( $val:literal )) => { Token::Literal(Literal::Integer($val.into())) };
        (@literal Char ( $val:literal )) => { Token::Literal(Literal::Char($val)) };
        (@literal String ( $val:literal )) => { Token::Literal(Literal::String($val.into())) };
    }

    macro_rules! assert_error {
        ($source:expr, { $l1:literal : $c1:literal - $l2:literal : $c2:literal : $e:ident $( ( $($a:tt)* ) )? } $(,)?) => {{
            let tokens = tokenize($source);
            assert_eq!(tokens.is_err(), true, "Expected tokenization to fail but succeeded");
            assert_eq!(tokens.unwrap_err(), assert_error!(@err $e ( Span::from((($l1, $c1), ($l2, $c2))) $( , $($a)* )? )));
        }};

        (@err InvalidToken ( $span:expr )) => { Error::InvalidToken($span) };
        (@err Unterminated ( $span:expr , $what:expr )) => { Error::Unterminated($span, $what.into()) };
        (@err EmptyChar ( $span:expr )) => { Error::EmptyChar($span) };
        (@err InvalidChar ( $span:expr )) => { Error::InvalidChar($span) };
        (@err InvalidEscapeChar ( $span:expr )) => { Error::InvalidEscapeChar($span) };
    }

    fn tokenize(source: &str) -> Result<Vec<(Span, Token)>> {
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        loop {
            match lexer.next_token() {
                Ok((token, span)) => {
                    if token == Token::Eof {
                        tokens.push((span, Token::Eof));
                        break Ok(tokens);
                    }
                    tokens.push((span, token));
                }
                Err(e) => break Err(e),
            }
        }
    }

    #[test]
    fn test_empty_input() {
        assert_tokens!("", { 1:1-1:1: Eof });
    }

    #[test]
    fn test_whitespace() {
        assert_tokens!("   \t\n\r  \n \u{00A0} \u{3000} \u{2003}", { 3:7-3:7: Eof });
    }

    #[test]
    fn test_comments() {
        assert_tokens!(
            indoc! {"
                // comment
                // comment
            "},
            {
                3:1-3:1: Eof
            },
        );
        assert_tokens!(
            indoc! {"
                // comment
                // comment
                x
            "},
            {
                3:1-3:1: Identifier("x")
                4:1-4:1: Eof
            },
        );
        assert_tokens!(
            indoc! {"
                // comment
                x
                // comment
                y
            "},
            {
                2:1-2:1: Identifier("x")
                4:1-4:1: Identifier("y")
                5:1-5:1: Eof
            },
        );
    }

    #[test]
    fn test_symbols() {
        assert_tokens!(
            indoc! {"
                +
                -
                *
                /
                ->
                +-*/->
            "},
            {
                1:1-1:1: Symbol(+)
                2:1-2:1: Symbol(-)
                3:1-3:1: Symbol(*)
                4:1-4:1: Symbol(/)
                5:1-5:2: Symbol(->)
                6:1-6:6: Symbol(+-*/->)
                7:1-7:1: Eof
            }
        );
    }

    #[test]
    fn test_delimiters() {
        assert_tokens!(
            indoc! {"
                (
                )
                [
                ]
                ,
                ;
            "},
            {
                1:1-1:1: Delimiter(ParenLeft)
                2:1-2:1: Delimiter(ParenRight)
                3:1-3:1: Delimiter(BracketLeft)
                4:1-4:1: Delimiter(BracketRight)
                5:1-5:1: Delimiter(Comma)
                6:1-6:1: Delimiter(Semicolon)
                7:1-7:1: Eof
            }
        );
    }

    #[test]
    fn test_keywords() {
        assert_tokens!(
            indoc! {"
                op
                left
                right
                none
                import
                let
                if
                then
                else
            "},
            {
                1:1-1:2: Keyword(Op)
                2:1-2:4: Keyword(Left)
                3:1-3:5: Keyword(Right)
                4:1-4:4: Keyword(None)
                5:1-5:6: Keyword(Import)
                6:1-6:3: Keyword(Let)
                7:1-7:2: Keyword(If)
                8:1-8:4: Keyword(Then)
                9:1-9:4: Keyword(Else)
                10:1-10:1: Eof
            }
        );
    }

    #[test]
    fn test_identifiers() {
        assert_tokens!(
            indoc! {"
                foo
                _foo
                foo_
                foo_bar
                foo123
                _foo123
                foo123_
                foo_bar_123
                x\u{0664}
                α
                β
                γ
                δ
                hello_世界
                _привет
                operator
                lettuce
            "},
            {
                1:1-1:3: Identifier("foo")
                2:1-2:4: Identifier("_foo")
                3:1-3:4: Identifier("foo_")
                4:1-4:7: Identifier("foo_bar")
                5:1-5:6: Identifier("foo123")
                6:1-6:7: Identifier("_foo123")
                7:1-7:7: Identifier("foo123_")
                8:1-8:11: Identifier("foo_bar_123")
                9:1-9:2: Identifier("x\u{0664}")
                10:1-10:1: Identifier("α")
                11:1-11:1: Identifier("β")
                12:1-12:1: Identifier("γ")
                13:1-13:1: Identifier("δ")
                14:1-14:8: Identifier("hello_世界")
                15:1-15:7: Identifier("_привет")
                16:1-16:8: Identifier("operator")
                17:1-17:7: Identifier("lettuce")
                18:1-18:1: Eof
            },
        );
    }

    #[test]
    fn test_integer_literals() {
        assert_tokens!(
            indoc! {"
                0
                42
                123456789
            "},
            {
                1:1-1:1: Literal(Integer("0"))
                2:1-2:2: Literal(Integer("42"))
                3:1-3:9: Literal(Integer("123456789"))
                4:1-4:1: Eof
            }
        );
    }

    #[test]
    fn test_char_literals() {
        assert_tokens!(
            indoc! {"
                'a'
                'α'
                '世'
                '🌍'
                '\\n'
                '\\''
            "},
            {
                1:1-1:3: Literal(Char('a'))
                2:1-2:3: Literal(Char('α'))
                3:1-3:3: Literal(Char('世'))
                4:1-4:3: Literal(Char('🌍'))
                5:1-5:4: Literal(Char('\n'))
                6:1-6:4: Literal(Char('\''))
                7:1-7:1: Eof
            }
        );
        assert_error!("''", { 1:1-1:2: EmptyChar });
        assert_error!("'ab'", { 1:1-1:4: InvalidChar });
        assert_error!("'a", { 1:1-1:1: Unterminated("'") });
        assert_error!("'\\x'", { 1:2-1:3: InvalidEscapeChar });
    }

    #[test]
    fn test_string_literals() {
        assert_tokens!(
            indoc! {r#"
                "hello"
                "world\n"
                "escaped \"quote\""
                "Hello 世界!"
                "Привет"
                "🌍"
            "#},
            {
                1:1-1:7: Literal(String("hello"))
                2:1-2:9: Literal(String("world\n"))
                3:1-3:19: Literal(String("escaped \"quote\""))
                4:1-4:11: Literal(String("Hello 世界!"))
                5:1-5:8: Literal(String("Привет"))
                6:1-6:3: Literal(String("🌍"))
                7:1-7:1: Eof
            }
        );
        assert_error!("\"hello", { 1:1-1:1: Unterminated("\"") });
        assert_error!("\"\\x\"", { 1:2-1:3: InvalidEscapeChar });
    }

    #[test]
    fn test_error_invalid_token() {
        assert_error!("\0", { 1:1-1:1: InvalidToken });
        assert_error!("\u{0301}", { 1:1-1:1: InvalidToken });
        assert_error!("\u{0664}", { 1:1-1:1: InvalidToken });
        assert_error!("\u{096D}", { 1:1-1:1: InvalidToken });
        assert_error!("★", { 1:1-1:1: InvalidToken });
        assert_error!("♥", { 1:1-1:1: InvalidToken });
        assert_error!("←", { 1:1-1:1: InvalidToken });
    }

    #[test]
    fn test_fib() {
        assert_tokens!(
            indoc! {"
                let fib = n ->
                    if n == 0 || n == 1 then n
                    else fib (n - 1) + fib (n - 2);
            "},
            {
                1:1-1:3: Keyword(Let)
                1:5-1:7: Identifier("fib")
                1:9-1:9: Symbol(=)
                1:11-1:11: Identifier("n")
                1:13-1:14: Symbol(->)
                2:5-2:6: Keyword(If)
                2:8-2:8: Identifier("n")
                2:10-2:11: Symbol(==)
                2:13-2:13: Literal(Integer("0"))
                2:15-2:16: Symbol(||)
                2:18-2:18: Identifier("n")
                2:20-2:21: Symbol(==)
                2:23-2:23: Literal(Integer("1"))
                2:25-2:28: Keyword(Then)
                2:30-2:30: Identifier("n")
                3:5-3:8: Keyword(Else)
                3:10-3:12: Identifier("fib")
                3:14-3:14: Delimiter(ParenLeft)
                3:15-3:15: Identifier("n")
                3:17-3:17: Symbol(-)
                3:19-3:19: Literal(Integer("1"))
                3:20-3:20: Delimiter(ParenRight)
                3:22-3:22: Symbol(+)
                3:24-3:26: Identifier("fib")
                3:28-3:28: Delimiter(ParenLeft)
                3:29-3:29: Identifier("n")
                3:31-3:31: Symbol(-)
                3:33-3:33: Literal(Integer("2"))
                3:34-3:34: Delimiter(ParenRight)
                3:35-3:35: Delimiter(Semicolon)
                4:1-4:1: Eof
            }
        );
    }
}
