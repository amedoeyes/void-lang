use fxhash::{FxHashMap, FxHashSet};
use itertools::Itertools;

use crate::{
    ast::{
        arena::NodeArena,
        node::Node,
        pattern::{Pattern, PrettyPattern},
    },
    span::Span,
};

pub fn missing(
    nodes: &NodeArena,
    type_ctors: &FxHashMap<String, FxHashMap<String, (usize, usize)>>,
    matrix: &[(Vec<Node>, Node)],
) -> Vec<PrettyPattern> {
    missing_inner(nodes, type_ctors, matrix, 1)
        .into_iter()
        .flatten()
        .collect()
}

fn missing_inner(
    nodes: &NodeArena,
    type_ctors: &FxHashMap<String, FxHashMap<String, (usize, usize)>>,
    matrix: &[(Vec<Node>, Node)],
    width: usize,
) -> Vec<Vec<PrettyPattern>> {
    if matrix.is_empty() {
        Vec::from([std::iter::repeat(PrettyPattern::Wildcard)
            .take(width)
            .collect()])
    } else if matrix.iter().any(|(p, _)| p.is_empty()) {
        Vec::new()
    } else {
        let used_ctors = used_constructors(nodes, matrix);
        let ctors = constructors(nodes, type_ctors, matrix);
        if !used_ctors.is_empty() {
            ctors
                .into_iter()
                .flat_map(|(name, (_, arity))| {
                    missing_inner(
                        nodes,
                        type_ctors,
                        &specialize(nodes, matrix, &name, arity),
                        arity + width - 1,
                    )
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
            missing_inner(nodes, type_ctors, &default(nodes, matrix), width - 1)
                .into_iter()
                .map(|row| [PrettyPattern::Wildcard].into_iter().chain(row).collect())
                .collect()
        }
    }
}

pub fn redundant(
    nodes: &NodeArena,
    type_ctors: &FxHashMap<String, FxHashMap<String, (usize, usize)>>,
    arms: &[(Node, Node)],
) -> Vec<(PrettyPattern, Span)> {
    arms.iter()
        .copied()
        .fold(
            (Vec::new(), Vec::new()),
            |(mut redundant, mut prev_matrix), (pattern, body)| {
                let mut new_matrix = prev_matrix.clone();
                for pat in expand_pattern(nodes, pattern) {
                    if !is_useful(nodes, type_ctors, &new_matrix, &[pat]) {
                        redundant.push((PrettyPattern::from_pattern(nodes, pat), nodes.span(pat)));
                    }
                    new_matrix.push((Vec::from([pat]), body));
                }
                prev_matrix.push((Vec::from([(pattern)]), body));
                (redundant, prev_matrix)
            },
        )
        .0
}

pub fn is_useful(
    nodes: &NodeArena,
    type_ctors: &FxHashMap<String, FxHashMap<String, (usize, usize)>>,
    matrix: &[(Vec<Node>, Node)],
    query: &[Node],
) -> bool {
    let matrix = matrix
        .iter()
        .filter(|(p, _)| {
            p.len() == query.len() && p.iter().zip(query).all(|(&a, &b)| compatible(nodes, a, b))
        })
        .cloned()
        .collect_vec();
    let matrix = matrix
        .iter()
        .enumerate()
        .filter(|(i, r_i)| {
            !matrix.iter().enumerate().any(|(j, r_j)| {
                *i != j
                    && r_j
                        .0
                        .iter()
                        .zip(r_i.0.iter())
                        .all(|(&a, &b)| subsumes(nodes, a, b))
                    && !r_i
                        .0
                        .iter()
                        .zip(r_j.0.iter())
                        .all(|(&a, &b)| subsumes(nodes, a, b))
            })
        })
        .map(|(_, r)| r.clone())
        .collect_vec();
    let useful = is_useful_inner(nodes, type_ctors, &matrix, query);
    useful
}

fn is_useful_inner(
    nodes: &NodeArena,
    type_ctors: &FxHashMap<String, FxHashMap<String, (usize, usize)>>,
    matrix: &[(Vec<Node>, Node)],
    query: &[Node],
) -> bool {
    if matrix.is_empty() {
        true
    } else if matrix.iter().any(|(p, _)| p.is_empty()) || query.is_empty() {
        false
    } else {
        let used_ctors = used_constructors(nodes, matrix);
        let ctors = constructors(nodes, type_ctors, matrix);
        match nodes
            .kind(query[0])
            .as_pattern()
            .expect("node should be pattern")
        {
            Pattern::Wildcard | Pattern::Identifier(..) => {
                if !used_ctors.is_empty() {
                    let mut res = ctors.is_empty();
                    for (name, (_, arity)) in ctors {
                        let new_matrix = specialize(nodes, matrix, &name, arity);
                        let new_query = std::iter::repeat(nodes.builtins.wildcard)
                            .take(arity)
                            .chain(query[1..].iter().cloned())
                            .collect_vec();
                        if is_useful_inner(nodes, type_ctors, &new_matrix, &new_query) {
                            res = true;
                            break;
                        }
                    }
                    res
                } else {
                    is_useful_inner(nodes, type_ctors, &default(nodes, matrix), &query[1..])
                }
            }
            Pattern::Constructor(name, subpats) => {
                let (_, arity) = ctors.get(name).copied().unwrap_or_default();
                let new_matrix = specialize(nodes, matrix, &name, arity);
                let new_query = subpats
                    .iter()
                    .copied()
                    .chain(query[1..].iter().copied())
                    .collect_vec();
                is_useful_inner(nodes, type_ctors, &new_matrix, &new_query)
            }
            Pattern::Or(alts) => {
                let mut res = false;
                let mut new_matrix = matrix.to_vec();
                for alt in alts {
                    let new_query = std::iter::once(*alt)
                        .into_iter()
                        .chain(query[1..].iter().cloned())
                        .collect_vec();
                    if is_useful_inner(nodes, type_ctors, &new_matrix, &new_query) {
                        res = true;
                        break;
                    }
                    let new_row = std::iter::once(*alt)
                        .into_iter()
                        .chain(matrix[0].0[1..].iter().copied())
                        .collect_vec();
                    new_matrix.push((new_row, matrix[0].1))
                }
                res
            }
        }
    }
}

pub fn default(nodes: &NodeArena, matrix: &[(Vec<Node>, Node)]) -> Vec<(Vec<Node>, Node)> {
    expand(nodes, matrix)
        .into_iter()
        .filter_map(|(p, b)| p.split_first().map(|(f, r)| ((*f, r.to_vec()), b)))
        .filter_map(|((f, r), b)| nodes.kind(f).as_pattern().map(|p| ((p, r.to_vec()), b)))
        .filter(|((f, _), _)| f.is_wildcard() || f.is_identifer())
        .map(|((_, r), b)| (r, b))
        .collect()
}

pub fn specialize(
    nodes: &NodeArena,
    matrix: &[(Vec<Node>, Node)],
    ctor: &str,
    arity: usize,
) -> Vec<(Vec<Node>, Node)> {
    expand(nodes, matrix)
        .into_iter()
        .filter_map(|(p, b)| p.split_first().map(|(f, r)| ((*f, r.to_vec()), b)))
        .filter_map(|((f, r), b)| nodes.kind(f).as_pattern().map(|p| (((f, p), r), b)))
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
        .collect()
}

pub fn identifiers(nodes: &NodeArena, matrix: &[(Vec<Node>, Node)]) -> FxHashSet<String> {
    expand(nodes, matrix)
        .into_iter()
        .filter_map(|(p, _)| p.first().copied())
        .filter_map(|p| nodes.kind(p).as_pattern().and_then(|p| p.as_identifier()))
        .map(|i| i.to_string())
        .collect()
}

fn compatible(nodes: &NodeArena, lhs: Node, rhs: Node) -> bool {
    let lhs_pat = nodes
        .kind(lhs)
        .as_pattern()
        .expect("node should be pattern");
    let rhs_pat = nodes
        .kind(rhs)
        .as_pattern()
        .expect("node should be pattern");
    match (lhs_pat, rhs_pat) {
        (Pattern::Wildcard, _) | (_, Pattern::Wildcard) => true,
        (Pattern::Identifier(..), _) | (_, Pattern::Identifier(..)) => true,
        (Pattern::Constructor(name1, subpats1), Pattern::Constructor(name2, subpats2)) => {
            name1 == name2
                && subpats1.len() == subpats2.len()
                && subpats1
                    .iter()
                    .zip(subpats2)
                    .all(|(&a, &b)| compatible(nodes, a, b))
        }
        (Pattern::Or(alts), _) => alts.iter().any(|&alt| compatible(nodes, alt, rhs)),
        (_, Pattern::Or(alts)) => alts.iter().any(|&alt| compatible(nodes, lhs, alt)),
    }
}

fn subsumes(nodes: &NodeArena, lhs: Node, rhs: Node) -> bool {
    let lhs_pat = nodes
        .kind(lhs)
        .as_pattern()
        .expect("node should be pattern");
    let rhs_pat = nodes
        .kind(rhs)
        .as_pattern()
        .expect("node should be pattern");
    match (lhs_pat, rhs_pat) {
        (Pattern::Wildcard, _) => true,
        (Pattern::Identifier(..), _) => true,
        (Pattern::Constructor(name1, subpats1), Pattern::Constructor(name2, subpats2)) => {
            name1 == name2
                && subpats1.len() == subpats2.len()
                && subpats1
                    .iter()
                    .zip(subpats2)
                    .all(|(&a, &b)| subsumes(nodes, a, b))
        }
        (Pattern::Or(alts), _) => alts.iter().any(|&alt| subsumes(nodes, alt, rhs)),
        (_, Pattern::Or(alts)) => alts.iter().all(|&alt| subsumes(nodes, lhs, alt)),
        _ => false,
    }
}

fn expand_pattern(nodes: &NodeArena, pattern: Node) -> Vec<Node> {
    let mut res = Vec::new();
    match nodes.kind(pattern).as_pattern() {
        Some(Pattern::Or(alts)) => res.extend(alts.iter().flat_map(|&p| expand_pattern(nodes, p))),
        _ => res.push(pattern),
    }
    res
}

fn expand(nodes: &NodeArena, matrix: &[(Vec<Node>, Node)]) -> Vec<(Vec<Node>, Node)> {
    matrix
        .iter()
        .filter_map(|(p, b)| p.split_first().map(|p| (p, b)))
        .flat_map(|((f, r), b)| {
            expand_pattern(nodes, *f)
                .iter()
                .map(|&n| std::iter::once(n).chain(r.iter().copied()).collect_vec())
                .map(|p| (p, *b))
                .collect_vec()
        })
        .collect()
}

fn constructors(
    nodes: &NodeArena,
    type_ctors: &FxHashMap<String, FxHashMap<String, (usize, usize)>>,
    matrix: &[(Vec<Node>, Node)],
) -> FxHashMap<String, (usize, usize)> {
    matrix
        .first()
        .and_then(|(p, _)| p.first().copied())
        .and_then(|p| nodes.ty(p))
        .and_then(|t| t.as_adt())
        .and_then(|(n, _)| type_ctors.get(n).cloned())
        .unwrap_or_default()
}

fn used_constructors<'a>(nodes: &'a NodeArena, matrix: &[(Vec<Node>, Node)]) -> FxHashSet<&'a str> {
    matrix
        .iter()
        .filter_map(|(p, _)| p.first().copied())
        .flat_map(|p| expand_pattern(nodes, p))
        .flat_map(|p| nodes.kind(p).as_pattern().and_then(|p| p.as_constructor()))
        .map(|(n, _)| n)
        .collect()
}
