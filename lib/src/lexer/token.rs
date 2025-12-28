use std::fmt::Display;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    BracketLeft,
    BracketRight,
    Comma,
    Equal,
    ParenLeft,
    ParenRight,
    Semicolon,
    HyphenGreaterThan,

    Boolean(String),
    Integer(String),
    Char(char),
    String(String),
    Identifier(String),
    Operator(String),

    Op,
    Left,
    Right,
    None,

    Import,
    Let,
    If,
    Then,
    Else,

    Eof,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Token::BracketLeft => write!(f, "["),
            Token::BracketRight => write!(f, "]"),
            Token::Comma => write!(f, ","),
            Token::Equal => write!(f, "="),
            Token::HyphenGreaterThan => write!(f, "->"),
            Token::ParenLeft => write!(f, "("),
            Token::ParenRight => write!(f, ")"),
            Token::Semicolon => write!(f, ";"),
            Token::Boolean(val) => write!(f, "{val}"),
            Token::Integer(val) => write!(f, "{val}"),
            Token::Char(val) => write!(f, "'{val}'"),
            Token::String(val) => write!(f, "\"{val}\""),
            Token::Identifier(val) => write!(f, "{val}"),
            Token::Operator(op) => write!(f, "({op})"),
            Token::Op => write!(f, "op"),
            Token::Left => write!(f, "left"),
            Token::Right => write!(f, "right"),
            Token::None => write!(f, "none"),
            Token::Import => write!(f, "import"),
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
            Token::Then => write!(f, "then"),
            Token::Else => write!(f, "else"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

pub const SYMBOLS: &[(&str, Token)] = &[
    ("->", Token::HyphenGreaterThan),
    ("(", Token::ParenLeft),
    (")", Token::ParenRight),
    (",", Token::Comma),
    (";", Token::Semicolon),
    ("=", Token::Equal),
    ("[", Token::BracketLeft),
    ("]", Token::BracketRight),
];

pub const KEYWORDS: &[(&str, Token)] = &[
    ("op", Token::Op),
    ("left", Token::Left),
    ("right", Token::Right),
    ("none", Token::None),
    ("import", Token::Import),
    ("let", Token::Let),
    ("if", Token::If),
    ("then", Token::Then),
    ("else", Token::Else),
];
