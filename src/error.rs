use core::num;
use std::io;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Clap(#[from] clap::Error),

    #[error(transparent)]
    Io(#[from] io::Error),

    #[error(transparent)]
    ParseInt(#[from] num::ParseIntError),
}

pub type Result<T> = std::result::Result<T, Error>;
