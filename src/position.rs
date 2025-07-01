use core::fmt;

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
        self.end = other.end;
        self
    }
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}
