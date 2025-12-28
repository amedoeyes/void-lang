use crate::span::Span;
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub enum Error {
    InvalidToken(Span),
    Unterminated(String, Span),
    EmptyChar(Span),
    InvalidChar(Span),
    InvalidEscapeChar(Span),
}

pub type Result<T> = std::result::Result<T, Error>;

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Error::InvalidToken(_) => write!(f, "invalid token"),
            Error::Unterminated(what, _) => write!(f, "unterminated {}", what),
            Error::EmptyChar(_) => write!(f, "empty char"),
            Error::InvalidChar(_) => write!(f, "invalid char"),
            Error::InvalidEscapeChar(_) => write!(f, "invalid escape char"),
        }
    }
}
