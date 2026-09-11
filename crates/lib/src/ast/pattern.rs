use std::fmt::{self, Display, Formatter};

use fxhash::FxHashSet;
use itertools::Itertools;

use crate::ast::{arena::NodeArena, node::Node};

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Identifier(String),
    Constructor(String, Vec<Node>),
    Or(Vec<Node>),
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

    pub fn constructors(&self, nodes: &NodeArena) -> FxHashSet<String> {
        let mut res = FxHashSet::default();
        match self {
            Pattern::Constructor(name, _) => {
                res.insert(name.clone());
            }
            Pattern::Or(alts) => {
                for alt in alts {
                    res.extend(
                        nodes
                            .kind(*alt)
                            .as_pattern()
                            .expect("node should be pattern")
                            .constructors(nodes),
                    );
                }
            }
            _ => {}
        }
        res
    }

    pub fn compatible(&self, nodes: &NodeArena, rhs: &Pattern) -> bool {
        match (self, rhs) {
            (Pattern::Wildcard, _) | (_, Pattern::Wildcard) => true,
            (Pattern::Identifier(..), _) | (_, Pattern::Identifier(..)) => true,
            (Pattern::Constructor(name1, subpats1), Pattern::Constructor(name2, subpats2)) => {
                name1 == name2
                    && subpats1.len() == subpats2.len()
                    && subpats1
                        .iter()
                        .zip(subpats2)
                        .map(|(&a, &b)| {
                            (
                                nodes.kind(a).as_pattern().expect("node should be pattern"),
                                nodes.kind(b).as_pattern().expect("node should be pattern"),
                            )
                        })
                        .all(|(a, b)| a.compatible(nodes, b))
            }
            (Pattern::Or(alts), b) => alts
                .iter()
                .map(|&a| nodes.kind(a).as_pattern().expect("node should be pattern"))
                .any(|a| a.compatible(nodes, b)),
            (a, Pattern::Or(alts)) => alts
                .iter()
                .map(|&b| nodes.kind(b).as_pattern().expect("node should be pattern"))
                .any(|b| a.compatible(nodes, b)),
        }
    }

    pub fn subsumes(&self, nodes: &NodeArena, rhs: &Pattern) -> bool {
        match (self, rhs) {
            (Pattern::Wildcard, _) => true,
            (Pattern::Identifier(..), _) => true,
            (Pattern::Constructor(name1, subpats1), Pattern::Constructor(name2, subpats2)) => {
                name1 == name2
                    && subpats1.len() == subpats2.len()
                    && subpats1
                        .iter()
                        .zip(subpats2)
                        .map(|(&a, &b)| {
                            (
                                nodes.kind(a).as_pattern().expect("node should be pattern"),
                                nodes.kind(b).as_pattern().expect("node should be pattern"),
                            )
                        })
                        .all(|(a, b)| a.subsumes(nodes, b))
            }
            (Pattern::Or(alts), b) => alts
                .iter()
                .map(|&a| nodes.kind(a).as_pattern().expect("node should be pattern"))
                .any(|a| a.subsumes(nodes, b)),
            (a, Pattern::Or(alts)) => alts
                .iter()
                .map(|&b| nodes.kind(b).as_pattern().expect("node should be pattern"))
                .all(|b| a.subsumes(nodes, b)),
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub enum PrettyPattern {
    Wildcard,
    Identifier(String),
    Constructor(String, Vec<PrettyPattern>),
    Or(Vec<PrettyPattern>),
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
            Pattern::Or(alts) => PrettyPattern::Or(
                alts.into_iter()
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
            PrettyPattern::Or(alts) => write!(f, "{}", alts.iter().format(" | ")),
        }
    }
}
