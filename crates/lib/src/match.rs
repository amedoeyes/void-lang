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
pub struct Match<'a> {
    nodes: &'a NodeArena,
    type_ctors: &'a FxHashMap<String, FxHashMap<String, (usize, usize)>>,
    scrutinee: Node,
    arms: Vec<(Vec<Node>, Node)>,
}

impl<'a> Match<'a> {
    pub fn new(
        nodes: &'a NodeArena,
        type_ctors: &'a FxHashMap<String, FxHashMap<String, (usize, usize)>>,
        scrutinee: Node,
        arms: Vec<(Vec<Node>, Node)>,
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
        if self.is_empty() {
            Vec::from([std::iter::repeat(PrettyPattern::Wildcard)
                .take(width)
                .collect()])
        } else if self.iter().any(|(p, _)| p.is_empty()) {
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
                        new_match.push((Vec::from([pat]), body));
                    }
                    prev_match.push((Vec::from([(pattern)]), body));
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
            .iter()
            .filter(|(p, _)| {
                p.len() == query.len()
                    && p.iter()
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
            .iter()
            .enumerate()
            .filter(|(i, r_i)| {
                !new_match.arms.iter().enumerate().any(|(j, r_j)| {
                    *i != j
                        && r_j
                            .0
                            .iter()
                            .zip(r_i.0.iter())
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
                            .0
                            .iter()
                            .zip(r_j.0.iter())
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
        if self.is_empty() {
            true
        } else if self.iter().any(|(p, _)| p.is_empty()) || query.is_empty() {
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
                            .chain(self.arms[0].0[1..].iter().copied())
                            .collect_vec();
                        new_match.push((new_row, self.arms[0].1))
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
        self.iter()
            .filter_map(|(p, _)| p.first().copied())
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
            .into_iter()
            .filter_map(|(p, _)| p.first().copied())
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
        self.first()
            .and_then(|(p, _)| p.first().copied())
            .and_then(|p| self.nodes.ty(p))
            .expect("node should have type")
    }

    pub fn default(&self) -> Self {
        Self::new(
            self.nodes,
            self.type_ctors,
            self.scrutinee,
            self.expand()
                .into_iter()
                .filter_map(|(p, b)| p.split_first().map(|(f, r)| ((*f, r.to_vec()), b)))
                .filter_map(|((f, r), b)| {
                    self.nodes
                        .kind(f)
                        .as_pattern()
                        .map(|p| ((p, r.to_vec()), b))
                })
                .filter(|((f, _), _)| f.is_wildcard() || f.is_identifer())
                .map(|((_, r), b)| (r, b))
                .collect(),
        )
    }

    pub fn specialize(&self, ctor: &str, arity: usize) -> Self {
        Self::new(
            self.nodes,
            self.type_ctors,
            self.scrutinee,
            self.expand()
                .into_iter()
                .filter_map(|(p, b)| p.split_first().map(|(f, r)| ((*f, r.to_vec()), b)))
                .filter_map(|((f, r), b)| self.nodes.kind(f).as_pattern().map(|p| (((f, p), r), b)))
                .filter_map(|(((node, first), rest), body)| match first {
                    Pattern::Wildcard | Pattern::Identifier(..) => Some((
                        std::iter::repeat(node).take(arity).chain(rest).collect(),
                        body,
                    )),
                    Pattern::Constructor(name, subpats) if name == ctor => {
                        Some((subpats.iter().copied().chain(rest).collect(), body))
                    }
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
            self.iter()
                .filter_map(|(p, b)| p.split_first().map(|p| (p, b)))
                .flat_map(|((f, r), b)| {
                    self.expand_pattern(*f)
                        .iter()
                        .map(|&n| std::iter::once(n).chain(r.iter().copied()).collect_vec())
                        .map(|p| (p, *b))
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

    pub fn scrutinee(&self) -> Node {
        self.scrutinee
    }

    pub fn iter(&self) -> impl Iterator<Item = &(Vec<Node>, Node)> {
        self.arms.iter()
    }

    pub fn into_iter(self) -> impl Iterator<Item = (Vec<Node>, Node)> {
        self.arms.into_iter()
    }

    pub fn is_empty(&self) -> bool {
        self.arms.is_empty()
    }

    pub fn push(&mut self, row: (Vec<Node>, Node)) {
        self.arms.push(row)
    }

    pub fn first(&self) -> Option<&(Vec<Node>, Node)> {
        self.arms.first()
    }
}
