use crate::{lexer::Token, position::Spanned};

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixOp {
    Neg,
}

impl PrefixOp {
    pub fn from_token(kind: &Token) -> Option<Self> {
        match kind {
            Token::Hyphen => Some(PrefixOp::Neg),
            _ => None,
        }
    }

    pub fn precedence(&self) -> (u8, u8) {
        match self {
            PrefixOp::Neg => (0, 9),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lam,
}

impl InfixOp {
    pub fn from_token(kind: &Token) -> Option<Self> {
        match kind {
            Token::Plus => Some(InfixOp::Add),
            Token::Hyphen => Some(InfixOp::Sub),
            Token::Star => Some(InfixOp::Mul),
            Token::Slash => Some(InfixOp::Div),
            Token::EqualEqual => Some(InfixOp::Eq),
            Token::HyphenGreaterThan => Some(InfixOp::Lam),
            _ => None,
        }
    }

    pub fn precedence(&self) -> (u8, u8) {
        match self {
            InfixOp::Lam => (2, 1),
            InfixOp::Eq => (3, 4),
            InfixOp::Add | InfixOp::Sub => (5, 6),
            InfixOp::Mul | InfixOp::Div => (7, 8),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Boolean(bool),
    Integer(i64),
    Identifier(String),
    Condition {
        cond: Box<Spanned<Expr>>,
        then: Box<Spanned<Expr>>,
        alt: Box<Spanned<Expr>>,
    },
    Infix {
        lhs: Box<Spanned<Expr>>,
        op: InfixOp,
        rhs: Box<Spanned<Expr>>,
    },
    Prefix {
        op: PrefixOp,
        rhs: Box<Spanned<Expr>>,
    },
    Application {
        func: Box<Spanned<Expr>>,
        arg: Box<Spanned<Expr>>,
    },
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let { name: String, value: Expr },
    Expr(Expr),
}

pub struct Program(pub Vec<Spanned<Stmt>>);
