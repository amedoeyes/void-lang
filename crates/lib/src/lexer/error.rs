use crate::span::Span;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, PartialEq)]
pub enum Error {
    InvalidToken(Span),
    Unterminated(Span, String),
    EmptyChar(Span),
    InvalidChar(Span),
    InvalidEscapeChar(Span),
}

pub type Result<T> = std::result::Result<T, Error>;

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Error::InvalidToken(_) => write!(f, "invalid token"),
            Error::Unterminated(_, what) => write!(f, "unterminated {}", what),
            Error::EmptyChar(_) => write!(f, "empty char"),
            Error::InvalidChar(_) => write!(f, "invalid char"),
            Error::InvalidEscapeChar(_) => write!(f, "invalid escape char"),
        }
    }
}
