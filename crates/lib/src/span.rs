use std::fmt::Debug;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub const DUMMY: Self = Self { line: 0, column: 0 };

    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub const DUMMY: Self = Self {
        start: Position::DUMMY,
        end: Position::DUMMY,
    };

    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn merge(self, other: Self) -> Self {
        if self == Self::DUMMY {
            other
        } else if other == Self::DUMMY {
            self
        } else {
            Span {
                start: self.start.min(other.start),
                end: self.end.max(other.end),
            }
        }
    }
}

impl From<((usize, usize), (usize, usize))> for Span {
    fn from(((l1, c1), (l2, c2)): ((usize, usize), (usize, usize))) -> Self {
        Self::new(Position::new(l1, c1), Position::new(l2, c2))
    }
}
