use core::fmt;
use std::io;

use crate::{eval, lexer::Token, parser, span::Span, type_system};

#[derive(Debug)]
pub enum Error {
    Clap(clap::Error),
    Io(io::Error),
    Parser(String, String, Box<parser::Error>),
    Type(String, String, Box<type_system::Error>),
    Eval(String, String, eval::Error),
}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Error::Io(value)
    }
}

impl From<clap::Error> for Error {
    fn from(value: clap::Error) -> Self {
        Error::Clap(value)
    }
}

fn write_message(f: &mut fmt::Formatter, filename: &str, span: Span, message: &str) -> fmt::Result {
    writeln!(
        f,
        "{}:{}:{}: {}",
        filename, span.start.line, span.start.column, message
    )
}

fn write_lines(f: &mut fmt::Formatter, source: &str, span: Span) -> fmt::Result {
    let lines = source
        .lines()
        .skip(span.start.line - 1)
        .take(span.end.line - span.start.line + 1)
        .collect::<Vec<&str>>();

    writeln!(f, "     | ")?;
    if lines.len() > 1 {
        for (i, line) in lines.iter().enumerate() {
            let line_num = i + span.start.line;
            writeln!(f, "{line_num:4} | {line}")?;
            if span.start.line == line_num {
                writeln!(
                    f,
                    "     | {}{}",
                    " ".repeat(span.start.column.saturating_sub(1)),
                    "^".repeat(span.start.column.abs_diff(line.len()) + 1)
                )?;
            } else if span.end.line == line_num {
                writeln!(f, "     | {}", "^".repeat(span.end.column))?;
            } else {
                writeln!(f, "     | {}", "^".repeat(line.len()))?;
            }
        }
    } else {
        writeln!(f, "{:4} | {}", span.start.line, lines.first().unwrap())?;
        writeln!(
            f,
            "     | {}{}",
            " ".repeat(span.start.column.saturating_sub(1)),
            "^".repeat(span.start.column.abs_diff(span.end.column) + 1)
        )?;
    }

    Ok(())
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Clap(err) => err.fmt(f),
            Error::Io(err) => err.fmt(f),

            Error::Parser(filename, source, err) => match err.as_ref() {
                parser::Error::UnexpectedToken(expected, (token, span)) => {
                    write_message(
                        f,
                        filename,
                        *span,
                        &format!("expected '{}' but got '{}'", expected, *token),
                    )?;
                    if *token != Token::Eof {
                        write_lines(f, source, *span)?;
                    }
                    Ok(())
                }
            },

            Error::Type(filename, source, err) => match err.as_ref() {
                type_system::Error::TypeMismatch((t1, s1), (t2, s2)) => {
                    write_message(f, filename, *s1, &format!("type mismatch: expected '{t1}'"))?;
                    write_lines(f, source, *s1)?;
                    write_message(f, filename, *s2, &format!("found '{t2}'"))?;
                    write_lines(f, source, *s2)
                }

                type_system::Error::InfiniteType(ty, span) => {
                    write_message(f, filename, *span, &format!("infinite type '{ty}'"))?;
                    write_lines(f, source, *span)
                }

                type_system::Error::UnknownIdentifier(id, span) => {
                    write_message(f, filename, *span, &format!("unknown identifier '{id}'"))?;
                    write_lines(f, source, *span)
                }
            },

            Error::Eval(filename, source, err) => match err {
                eval::Error::DivisionByZero(span) => {
                    write_message(f, filename, *span, "division by zero")?;
                    write_lines(f, source, *span)
                }
            },
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;
