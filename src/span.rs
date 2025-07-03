use core::fmt;
use std::fmt::Debug;

#[derive(Debug, Clone, Copy, Default)]
pub struct Position {
    pub index: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Copy, Default)]
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

    pub fn merge(self, other: &Self) -> Self {
        let mut span = self;
        if span.end.index < other.end.index {
            span.end = other.end;
        }
        if span.start.index > other.start.index {
            span.start = other.start;
        }
        span
    }
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Self {
        Self { value, span }
    }
}
