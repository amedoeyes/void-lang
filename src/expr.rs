use core::fmt;

use crate::{context::NodeId, lexer::Token};

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixOp {
    Neg,
    Not,
}

impl PrefixOp {
    pub fn from_token(kind: &Token) -> Option<Self> {
        match kind {
            Token::Hyphen => Some(PrefixOp::Neg),
            Token::Bang => Some(PrefixOp::Not),
            _ => None,
        }
    }

    pub fn precedence(&self) -> (u8, u8) {
        match self {
            PrefixOp::Neg | PrefixOp::Not => (0, 13),
        }
    }
}

impl fmt::Display for PrefixOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PrefixOp::Neg => write!(f, "-"),
            PrefixOp::Not => write!(f, "!"),
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
    Neq,

    And,
    Or,

    Lt,
    Gt,
    Lte,
    Gte,
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
            Token::BangEqual => Some(InfixOp::Neq),

            Token::AmpersandAmpersand => Some(InfixOp::And),
            Token::PipePipe => Some(InfixOp::Or),

            Token::LessThan => Some(InfixOp::Lt),
            Token::GreaterThan => Some(InfixOp::Gt),
            Token::LessThanEqual => Some(InfixOp::Lte),
            Token::GreaterThanEqual => Some(InfixOp::Gte),

            _ => None,
        }
    }

    pub fn precedence(&self) -> (u8, u8) {
        match self {
            InfixOp::Or => (1, 2),
            InfixOp::And => (3, 4),
            InfixOp::Eq | InfixOp::Neq => (5, 6),
            InfixOp::Lt | InfixOp::Gt | InfixOp::Lte | InfixOp::Gte => (7, 8),
            InfixOp::Add | InfixOp::Sub => (9, 10),
            InfixOp::Mul | InfixOp::Div | InfixOp::Mod => (11, 12),
        }
    }
}

impl fmt::Display for InfixOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            InfixOp::Add => write!(f, "+"),
            InfixOp::Sub => write!(f, "-"),
            InfixOp::Mul => write!(f, "*"),
            InfixOp::Div => write!(f, "/"),
            InfixOp::Mod => write!(f, "%"),

            InfixOp::Eq => write!(f, "=="),
            InfixOp::Neq => write!(f, "!="),

            InfixOp::And => write!(f, "&&"),
            InfixOp::Or => write!(f, "||"),

            InfixOp::Lt => write!(f, "<"),
            InfixOp::Gt => write!(f, ">"),
            InfixOp::Lte => write!(f, "<="),
            InfixOp::Gte => write!(f, ">="),
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
        cond: NodeId,
        then: NodeId,
        alt: NodeId,
    },
    Infix {
        lhs: NodeId,
        op: InfixOp,
        rhs: NodeId,
    },
    Prefix {
        op: PrefixOp,
        rhs: NodeId,
    },
    Lambda {
        param: NodeId,
        body: NodeId,
    },
    Application {
        func: NodeId,
        arg: NodeId,
    },
}
