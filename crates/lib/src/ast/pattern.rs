use std::fmt::{self, Display, Formatter};

use crate::ast::{arena::NodeArena, node::Node};

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Identifier(String),
    Constructor(String, Vec<Node>),
}

impl Pattern {
    pub const fn is_wildcard(&self) -> bool {
        matches!(self, Pattern::Wildcard)
    }

    pub const fn is_identifer(&self) -> bool {
        matches!(self, Pattern::Identifier(..))
    }

    pub const fn is_constructor(&self) -> bool {
        matches!(self, Pattern::Constructor(..))
    }

    pub fn as_identifier(&self) -> Option<&str> {
        match self {
            Pattern::Identifier(id) => Some(id.as_str()),
            _ => None,
        }
    }

    pub fn as_constructor(&self) -> Option<(&str, &[Node])> {
        match self {
            Pattern::Constructor(name, args) => Some((name.as_str(), args.as_slice())),
            _ => None,
        }
    }

    pub fn as_mut_identifier(&mut self) -> Option<&mut String> {
        match self {
            Pattern::Identifier(id) => Some(id),
            _ => None,
        }
    }

    pub fn as_mut_constructor(&mut self) -> Option<(&mut String, &mut Vec<Node>)> {
        match self {
            Pattern::Constructor(name, args) => Some((name, args)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum PrettyPattern {
    Wildcard,
    Identifier(String),
    Constructor(String, Vec<PrettyPattern>),
}

impl PrettyPattern {
    pub fn from_pattern(nodes: &NodeArena, pattern: Node) -> Self {
        match nodes
            .kind(pattern)
            .as_pattern()
            .cloned()
            .expect("node should be pattern")
        {
            Pattern::Wildcard => PrettyPattern::Wildcard,
            Pattern::Identifier(id) => Self::Identifier(id),
            Pattern::Constructor(name, subpats) => PrettyPattern::Constructor(
                name,
                subpats
                    .into_iter()
                    .map(|p| Self::from_pattern(nodes, p))
                    .collect(),
            ),
        }
    }
}

impl Display for PrettyPattern {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            PrettyPattern::Wildcard => write!(f, "_"),
            PrettyPattern::Identifier(id) => write!(f, "{id}"),
            PrettyPattern::Constructor(name, subpats) => {
                write!(f, "{name}")?;
                for pat in subpats.iter() {
                    write!(f, " ")?;
                    match pat {
                        Self::Constructor(_, subpats) if !subpats.is_empty() => {
                            write!(f, "({pat})")?;
                        }
                        _ => {
                            write!(f, "{pat}")?;
                        }
                    }
                }
                Ok(())
            }
        }
    }
}
