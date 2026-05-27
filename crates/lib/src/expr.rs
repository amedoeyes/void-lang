use crate::context::NodeId;
use itertools::Itertools;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    Char(char),
    Integer(i64),
    Constructor(String),
    Identifier(String),
    Match(NodeId, Vec<(Pattern, NodeId)>),
    Block(Vec<NodeId>),
    Lambda { param: String, body: NodeId },
    Application { func: NodeId, arg: NodeId },
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Identifier(String),
    Constructor(String, Vec<Pattern>),
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Pattern::Wildcard => write!(f, "_"),
            Pattern::Identifier(id) => write!(f, "{id}"),
            Pattern::Constructor(name, patterns) => {
                write!(
                    f,
                    "{}{}{}",
                    name,
                    if !patterns.is_empty() { " " } else { "" },
                    patterns.iter().map(|p| format!("{p}")).join(" ")
                )
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypeExpr {
    Unit,
    Identifier(String),
    Constructor(String, Vec<NodeId>),
}
