use std::fmt::Debug;

#[derive(Debug, Clone, Copy, Default)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn merge(self, other: Self) -> Self {
        let mut span = self;
        if other.start.line < span.start.line
            || (other.start.line == span.start.line && other.start.column < span.start.column)
        {
            span.start = other.start;
        }
        if other.end.line > span.end.line
            || (other.end.line == span.end.line && other.end.column > span.end.column)
        {
            span.end = other.end;
        }
        span
    }
}
