use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Delimiter(Delimiter),
    Literal(Literal),
    Keyword(Keyword),
    Identifier(String),
    Symbol(String),
    Eof,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    Op,
    Left,
    Right,
    None,
    Import,
    Let,
    If,
    Then,
    Else,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Bool(bool),
    Integer(String),
    Char(char),
    String(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Delimiter {
    ParenLeft,
    ParenRight,
    BracketLeft,
    BracketRight,
    Comma,
    Semicolon,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Token::Delimiter(delimiter) => delimiter.fmt(f),
            Token::Literal(literal) => literal.fmt(f),
            Token::Keyword(keyword) => keyword.fmt(f),
            Token::Identifier(val) => write!(f, "{val}"),
            Token::Symbol(val) => write!(f, "({val})"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Keyword::Op => write!(f, "op"),
            Keyword::Left => write!(f, "left"),
            Keyword::Right => write!(f, "right"),
            Keyword::None => write!(f, "none"),
            Keyword::Import => write!(f, "import"),
            Keyword::Let => write!(f, "let"),
            Keyword::If => write!(f, "if"),
            Keyword::Then => write!(f, "then"),
            Keyword::Else => write!(f, "else"),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Literal::Bool(val) => write!(f, "Bool({val})"),
            Literal::Integer(val) => write!(f, "{val}"),
            Literal::Char(val) => write!(f, "'{val}'"),
            Literal::String(val) => write!(f, "\"{val}\""),
        }
    }
}

impl Display for Delimiter {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Delimiter::ParenLeft => write!(f, "("),
            Delimiter::ParenRight => write!(f, ")"),
            Delimiter::BracketLeft => write!(f, "["),
            Delimiter::BracketRight => write!(f, "]"),
            Delimiter::Comma => write!(f, ","),
            Delimiter::Semicolon => write!(f, ";"),
        }
    }
}
