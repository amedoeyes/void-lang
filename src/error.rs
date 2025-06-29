use core::num;
use std::io;

use crate::lexer::{Span, TokenKind};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Clap(#[from] clap::Error),

    #[error(transparent)]
    Io(#[from] io::Error),

    #[error(transparent)]
    ParseInt(#[from] num::ParseIntError),

    #[error("expected '{0:#?}' but got '{1:#?}'")]
    UnexpectedToken(TokenKind, TokenKind, Span),
}

pub type Result<T> = std::result::Result<T, Error>;
