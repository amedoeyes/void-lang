use core::fmt;
use std::io;

use crate::{eval, lexer::Token, span::Span, type_system};

#[derive(Debug)]
pub enum SyntaxError {
    InvalidToken(Span),
    UnterminatedChar(Span),
    UnterminatedString(Span),
    EmptyChar(Span),
    InvalidChar(Span),
    InvalidEscapeChar(Span),
    UnexpectedToken(String, (Token, Span)),
}

#[derive(Debug)]
pub enum Error {
    Clap(clap::Error),
    Io(io::Error),
    Syntax(String, String, Box<SyntaxError>),
    Type(String, String, Box<type_system::Error>),
    Eval(String, String, eval::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

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

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::Clap(err) => err.fmt(f),
            Error::Io(err) => err.fmt(f),

            Error::Syntax(filename, source, err) => match err.as_ref() {
                SyntaxError::InvalidToken(span) => {
                    write_message_and_lines(f, filename, source, *span, "invalid token")
                }

                SyntaxError::UnterminatedChar(span) => {
                    write_message_and_lines(f, filename, source, *span, "unterminated char")
                }

                SyntaxError::UnterminatedString(span) => {
                    write_message_and_lines(f, filename, source, *span, "unterminated string")
                }

                SyntaxError::EmptyChar(span) => {
                    write_message_and_lines(f, filename, source, *span, "empty char")
                }

                SyntaxError::InvalidChar(span) => {
                    write_message_and_lines(f, filename, source, *span, "invalid char")
                }

                SyntaxError::InvalidEscapeChar(span) => {
                    write_message_and_lines(f, filename, source, *span, "invalid escape char")
                }

                SyntaxError::UnexpectedToken(expected, (token, span)) => {
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
                type_system::Error::TypeMismatch(ty1, ty2, span) => write_message_and_lines(
                    f,
                    filename,
                    source,
                    *span,
                    &format!("expected type '{ty1}' but found '{ty2}'"),
                ),
                type_system::Error::InfiniteType(ty, span) => write_message_and_lines(
                    f,
                    filename,
                    source,
                    *span,
                    &format!("infinite type '{ty}'"),
                ),
                type_system::Error::UnknownIdentifier(id, span) => write_message_and_lines(
                    f,
                    filename,
                    source,
                    *span,
                    &format!("unknown identifier '{id}'"),
                ),
                type_system::Error::UnknownOperator(op, span) => write_message_and_lines(
                    f,
                    filename,
                    source,
                    *span,
                    &format!("unknown operator '({op})'"),
                ),
                type_system::Error::InvalidOperator(op, ty, span) => write_message_and_lines(
                    f,
                    filename,
                    source,
                    *span,
                    &format!(
                        "operator '({op})' has type '{ty}' but expected a binary function type 'a -> b -> c'"
                    ),
                ),
                type_system::Error::NoInstance(cons, ty, span) => write_message_and_lines(
                    f,
                    filename,
                    source,
                    *span,
                    &format!("no '{cons}' instance for type '{ty}'"),
                ),
            },

            Error::Eval(filename, source, err) => match err {
                eval::Error::DivisionByZero(span) => {
                    write_message_and_lines(f, filename, source, *span, "division by zero")
                }
                eval::Error::EmptyList(span) => {
                    write_message_and_lines(f, filename, source, *span, "list is empty")
                }
                eval::Error::IO(message, span) => {
                    write_message_and_lines(f, filename, source, *span, message)
                }
            },
        }
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

fn write_message_and_lines(
    f: &mut fmt::Formatter,
    filename: &str,
    source: &str,
    span: Span,
    message: &str,
) -> fmt::Result {
    write_message(f, filename, span, message)?;
    write_lines(f, source, span)
}
