use std::io;

use crate::{eval, parser, type_system};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Clap(#[from] clap::Error),

    #[error(transparent)]
    Io(#[from] io::Error),

    #[error(transparent)]
    Parser(#[from] parser::Error),

    #[error(transparent)]
    Type(#[from] Box<type_system::Error>),

    #[error(transparent)]
    Eval(#[from] eval::Error),
}

pub type Result<T> = std::result::Result<T, Error>;
