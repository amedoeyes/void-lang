use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Delimiter(Delimiter),
    Literal(Literal),
    Keyword(Keyword),
    Type(String),
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
    Primitive,
    Type,
    Enum,
    Let,
    Match,
    With,
    If,
    Then,
    Else,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Integer(String),
    Char(char),
    String(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Delimiter {
    BraceLeft,
    BraceRight,
    BracketLeft,
    BracketRight,
    Comma,
    ParenLeft,
    ParenRight,
    Semicolon,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Token::Delimiter(delimiter) => delimiter.fmt(f),
            Token::Literal(literal) => literal.fmt(f),
            Token::Keyword(keyword) => keyword.fmt(f),
            Token::Type(val) => write!(f, "{val}"),
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
            Keyword::Primitive => write!(f, "primitive"),
            Keyword::Type => write!(f, "type"),
            Keyword::Enum => write!(f, "enum"),
            Keyword::Let => write!(f, "let"),
            Keyword::Match => write!(f, "Match"),
            Keyword::With => write!(f, "With"),
            Keyword::If => write!(f, "if"),
            Keyword::Then => write!(f, "then"),
            Keyword::Else => write!(f, "else"),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Literal::Integer(val) => write!(f, "{val}"),
            Literal::Char(val) => write!(f, "'{val}'"),
            Literal::String(val) => write!(f, "\"{val}\""),
        }
    }
}

impl Display for Delimiter {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Delimiter::BraceLeft => write!(f, "{{"),
            Delimiter::BraceRight => write!(f, "}}"),
            Delimiter::BracketLeft => write!(f, "["),
            Delimiter::BracketRight => write!(f, "]"),
            Delimiter::Comma => write!(f, ","),
            Delimiter::ParenLeft => write!(f, "("),
            Delimiter::ParenRight => write!(f, ")"),
            Delimiter::Semicolon => write!(f, ";"),
        }
    }
}
