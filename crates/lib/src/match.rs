use fxhash::{FxHashMap, FxHashSet};
use itertools::Itertools;

use crate::{
    ast::{
        arena::NodeArena,
        node::Node,
        pattern::{Pattern, PrettyPattern},
    },
    span::Span,
    type_system::Type,
};

#[derive(Debug, Clone)]
pub struct Arm {
    pub patterns: Vec<Node>,
    pub body: Node,
}

impl Arm {
    pub fn new(patterns: Vec<Node>, body: Node) -> Self {
        Self { patterns, body }
    }
}

#[derive(Debug, Clone)]
pub struct Match<'a> {
    nodes: &'a NodeArena,
    type_ctors: &'a FxHashMap<String, FxHashMap<String, (usize, usize)>>,
    pub scrutinee: Node,
    pub arms: Vec<Arm>,
}

impl<'a> Match<'a> {
    pub fn new(
        nodes: &'a NodeArena,
        type_ctors: &'a FxHashMap<String, FxHashMap<String, (usize, usize)>>,
        scrutinee: Node,
        arms: Vec<Arm>,
    ) -> Self {
        Self {
            nodes,
            type_ctors,
            scrutinee,
            arms,
        }
    }

    pub fn missing(&self) -> Vec<PrettyPattern> {
        self.missing_inner(1).into_iter().flatten().collect()
    }

    fn missing_inner(&self, width: usize) -> Vec<Vec<PrettyPattern>> {
        if self.arms.is_empty() {
            Vec::from([std::iter::repeat(PrettyPattern::Wildcard)
                .take(width)
                .collect()])
        } else if self.arms.iter().any(|arm| arm.patterns.is_empty()) {
            Vec::new()
        } else {
            let used_ctors = self.used_constructors();
            let ctors = self.constructors();
            if !used_ctors.is_empty() {
                ctors
                    .into_iter()
                    .flat_map(|(name, (_, arity))| {
                        self.specialize(&name, arity)
                            .missing_inner(arity + width - 1)
                            .into_iter()
                            .map(|mut row| {
                                let tail = row.split_off(arity.min(row.len()));
                                let padding = std::iter::repeat(PrettyPattern::Wildcard)
                                    .take(arity.saturating_sub(row.len()));
                                let ctor = PrettyPattern::Constructor(
                                    name.clone(),
                                    row.into_iter().chain(padding).collect(),
                                );
                                std::iter::once(ctor).chain(tail).collect()
                            })
                            .collect_vec()
                    })
                    .collect()
            } else {
                self.default()
                    .missing_inner(width - 1)
                    .into_iter()
                    .map(|row| [PrettyPattern::Wildcard].into_iter().chain(row).collect())
                    .collect()
            }
        }
    }

    pub fn redundant(
        nodes: &NodeArena,
        type_ctors: &FxHashMap<String, FxHashMap<String, (usize, usize)>>,
        scrutinee: Node,
        arms: &[(Node, Node)],
    ) -> Vec<(PrettyPattern, Span)> {
        arms.iter()
            .copied()
            .fold(
                (
                    Vec::new(),
                    Match::new(nodes, type_ctors, scrutinee, Vec::new()),
                ),
                |(mut redundant, mut prev_match), (pattern, body)| {
                    let mut new_match = prev_match.clone();
                    for pat in new_match.expand_pattern(pattern) {
                        if !new_match.is_useful(&[pat]) {
                            redundant
                                .push((PrettyPattern::from_pattern(nodes, pat), nodes.span(pat)));
                        }
                        new_match.arms.push(Arm::new(Vec::from([pat]), body));
                    }
                    prev_match.arms.push(Arm::new(Vec::from([(pattern)]), body));
                    (redundant, prev_match)
                },
            )
            .0
    }

    pub fn is_useful(&self, query: &[Node]) -> bool {
        self.safeguard(query).is_useful_inner(query)
    }

    fn safeguard(&self, query: &[Node]) -> Self {
        let mut new_match = self.clone();
        new_match.arms = new_match
            .arms
            .iter()
            .filter(|arm| {
                arm.patterns.len() == query.len()
                    && arm
                        .patterns
                        .iter()
                        .zip(query)
                        .map(|(&a, &b)| {
                            (
                                new_match
                                    .nodes
                                    .kind(a)
                                    .as_pattern()
                                    .expect("node should be pattern"),
                                new_match
                                    .nodes
                                    .kind(b)
                                    .as_pattern()
                                    .expect("node should be pattern"),
                            )
                        })
                        .all(|(a, b)| a.compatible(new_match.nodes, b))
            })
            .cloned()
            .collect_vec();
        new_match.arms = new_match
            .arms
            .iter()
            .enumerate()
            .filter(|(i, r_i)| {
                !new_match.arms.iter().enumerate().any(|(j, r_j)| {
                    *i != j
                        && r_j
                            .patterns
                            .iter()
                            .zip(r_i.patterns.iter())
                            .map(|(&a, &b)| {
                                (
                                    new_match
                                        .nodes
                                        .kind(a)
                                        .as_pattern()
                                        .expect("node should be pattern"),
                                    new_match
                                        .nodes
                                        .kind(b)
                                        .as_pattern()
                                        .expect("node should be pattern"),
                                )
                            })
                            .all(|(a, b)| a.subsumes(new_match.nodes, b))
                        && !r_i
                            .patterns
                            .iter()
                            .zip(r_j.patterns.iter())
                            .map(|(&a, &b)| {
                                (
                                    new_match
                                        .nodes
                                        .kind(a)
                                        .as_pattern()
                                        .expect("node should be pattern"),
                                    new_match
                                        .nodes
                                        .kind(b)
                                        .as_pattern()
                                        .expect("node should be pattern"),
                                )
                            })
                            .all(|(a, b)| a.subsumes(new_match.nodes, b))
                })
            })
            .map(|(_, r)| r.clone())
            .collect_vec();
        new_match
    }

    fn is_useful_inner(&self, query: &[Node]) -> bool {
        if self.arms.is_empty() {
            true
        } else if self.arms.iter().any(|arm| arm.patterns.is_empty()) || query.is_empty() {
            false
        } else {
            let used_ctors = self.used_constructors();
            let ctors = self.constructors();
            match self
                .nodes
                .kind(query[0])
                .as_pattern()
                .expect("node should be pattern")
            {
                Pattern::Wildcard | Pattern::Identifier(..) => {
                    if !used_ctors.is_empty() {
                        let mut res = ctors.is_empty();
                        for (name, (_, arity)) in ctors {
                            let new_query = std::iter::repeat(self.nodes.builtins.wildcard)
                                .take(arity)
                                .chain(query[1..].iter().cloned())
                                .collect_vec();
                            if self.specialize(&name, arity).is_useful_inner(&new_query) {
                                res = true;
                                break;
                            }
                        }
                        res
                    } else {
                        self.default().is_useful_inner(&query[1..])
                    }
                }
                Pattern::Constructor(name, subpats) => {
                    let (_, arity) = ctors.get(name).copied().unwrap_or_default();
                    let new_query = subpats
                        .iter()
                        .copied()
                        .chain(query[1..].iter().copied())
                        .collect_vec();
                    self.specialize(&name, arity).is_useful_inner(&new_query)
                }
                Pattern::Or(alts) => {
                    let mut res = false;
                    let mut new_match = self.clone();
                    for alt in alts {
                        let new_query = std::iter::once(*alt)
                            .into_iter()
                            .chain(query[1..].iter().cloned())
                            .collect_vec();
                        if new_match.is_useful_inner(&new_query) {
                            res = true;
                            break;
                        }
                        let new_row = std::iter::once(*alt)
                            .into_iter()
                            .chain(self.arms[0].patterns[1..].iter().copied())
                            .collect_vec();
                        new_match.arms.push(Arm::new(new_row, self.arms[0].body))
                    }
                    res
                }
            }
        }
    }

    pub fn constructors(&self) -> FxHashMap<String, (usize, usize)> {
        self.ty()
            .as_adt()
            .and_then(|(n, _)| self.type_ctors.get(n).cloned())
            .unwrap_or_default()
    }

    pub fn used_constructors(&self) -> FxHashSet<&'a str> {
        self.arms
            .iter()
            .filter_map(|arm| arm.patterns.first().copied())
            .flat_map(|p| self.expand_pattern(p))
            .flat_map(|p| {
                self.nodes
                    .kind(p)
                    .as_pattern()
                    .and_then(|p| p.as_constructor())
            })
            .map(|(n, _)| n)
            .collect()
    }

    pub fn identifiers(&self) -> FxHashSet<String> {
        self.expand()
            .arms
            .into_iter()
            .filter_map(|arm| arm.patterns.first().copied())
            .filter_map(|p| {
                self.nodes
                    .kind(p)
                    .as_pattern()
                    .and_then(|p| p.as_identifier())
            })
            .map(|i| i.to_string())
            .collect()
    }

    pub fn ty(&self) -> &Type {
        self.arms
            .first()
            .and_then(|arm| arm.patterns.first().copied())
            .and_then(|p| self.nodes.ty(p))
            .expect("node should have type")
    }

    pub fn default(&self) -> Self {
        Self::new(
            self.nodes,
            self.type_ctors,
            self.scrutinee,
            self.expand()
                .arms
                .into_iter()
                .filter_map(|arm| {
                    arm.patterns
                        .split_first()
                        .map(|(first, rest)| ((*first, rest.to_vec()), arm.body))
                })
                .filter_map(|((first, rest), body)| {
                    self.nodes
                        .kind(first)
                        .as_pattern()
                        .map(|p| ((p, rest.to_vec()), body))
                })
                .filter(|((first, _), _)| first.is_wildcard() || first.is_identifer())
                .map(|((_, rest), body)| Arm::new(rest, body))
                .collect(),
        )
    }

    pub fn specialize(&self, ctor: &str, arity: usize) -> Self {
        Self::new(
            self.nodes,
            self.type_ctors,
            self.scrutinee,
            self.expand()
                .arms
                .into_iter()
                .filter_map(|arm| {
                    arm.patterns
                        .split_first()
                        .map(|(first, rest)| ((*first, rest.to_vec()), arm.body))
                })
                .filter_map(|((first, rest), body)| {
                    self.nodes
                        .kind(first)
                        .as_pattern()
                        .map(|p| (((first, p), rest), body))
                })
                .filter_map(|(((node, first), rest), body)| match first {
                    Pattern::Wildcard | Pattern::Identifier(..) => Some(Arm::new(
                        std::iter::repeat(node).take(arity).chain(rest).collect(),
                        body,
                    )),
                    Pattern::Constructor(name, subpats) if name == ctor => Some(Arm::new(
                        subpats.iter().copied().chain(rest).collect(),
                        body,
                    )),
                    _ => None,
                })
                .collect(),
        )
    }

    fn expand(&self) -> Self {
        Self::new(
            self.nodes,
            self.type_ctors,
            self.scrutinee,
            self.arms
                .iter()
                .filter_map(|arm| arm.patterns.split_first().map(|p| (p, arm.body)))
                .flat_map(|((first, rest), body)| {
                    self.expand_pattern(*first)
                        .iter()
                        .map(|&n| std::iter::once(n).chain(rest.iter().copied()).collect_vec())
                        .map(|p| Arm::new(p, body))
                        .collect_vec()
                })
                .collect(),
        )
    }

    fn expand_pattern(&self, pattern: Node) -> Vec<Node> {
        let mut res = Vec::new();
        match self.nodes.kind(pattern).as_pattern() {
            Some(Pattern::Or(alts)) => {
                res.extend(alts.iter().flat_map(|&p| self.expand_pattern(p)))
            }
            _ => res.push(pattern),
        }
        res
    }
}
