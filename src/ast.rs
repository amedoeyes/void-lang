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
    Mod,
    Eq,
}

impl InfixOp {
    pub fn from_token(token: &Token) -> Option<Self> {
        match token {
            Token::Plus => Some(InfixOp::Add),
            Token::Hyphen => Some(InfixOp::Sub),
            Token::Star => Some(InfixOp::Mul),
            Token::Slash => Some(InfixOp::Div),
            Token::Percent => Some(InfixOp::Mod),
            Token::EqualEqual => Some(InfixOp::Eq),
            _ => None,
        }
    }

    pub fn precedence(&self) -> (u8, u8) {
        match self {
            InfixOp::Eq => (3, 4),
            InfixOp::Add | InfixOp::Sub => (5, 6),
            InfixOp::Mul | InfixOp::Div | InfixOp::Mod => (7, 8),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
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
    Lambda {
        param: String,
        body: Box<Spanned<Expr>>,
    },
    Application {
        func: Box<Spanned<Expr>>,
        arg: Box<Spanned<Expr>>,
    },
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Let { name: String, expr: Spanned<Expr> },
    Expr(Spanned<Expr>),
}
