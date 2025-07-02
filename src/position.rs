use core::fmt;
use std::fmt::Debug;

#[derive(Debug, Clone, Copy)]
pub struct Position {
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}", self.start.line, self.start.column)
    }
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn merge(&mut self, other: &Self) -> &Self {
        if self.end.index < other.end.index {
            self.end = other.end;
        }
        if self.start.index > other.start.index {
            self.start = other.start;
        }
        self
    }
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}
