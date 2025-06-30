use std::io;

use crate::{parser, type_system};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Clap(#[from] clap::Error),

    #[error(transparent)]
    Io(#[from] io::Error),

    #[error(transparent)]
    Parser(#[from] parser::Error),

    #[error(transparent)]
    Type(#[from] type_system::Error),
}

pub type Result<T> = std::result::Result<T, Error>;
