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
) -> Vec<Vec<PrettyPattern>> {
    if matrix.is_empty() {
        Vec::from([Vec::from([PrettyPattern::Wildcard])])
    } else if matrix.iter().any(|(r, _)| r.is_empty()) {
        Vec::new()
    } else {
        if let (default, _) = default(nodes, matrix)
            && !default.is_empty()
        {
            missing(nodes, type_ctors, &default)
                .into_iter()
                .map(|row| [PrettyPattern::Wildcard].into_iter().chain(row).collect())
                .collect()
        } else {
            nodes
                .ty(matrix[0].0[0])
                .expect("pattern must have type")
                .as_adt()
                .and_then(|(n, _)| type_ctors.get(n))
                .cloned()
                .unwrap_or_default()
                .into_iter()
                .flat_map(|(name, (_, arity))| {
                    missing(nodes, type_ctors, &specialize(nodes, matrix, &name, arity))
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
            |(mut redundant, mut prev_rows), (pattern, body)| {
                let is_useful = is_useful(
                    nodes,
                    type_ctors,
                    &prev_rows,
                    &[nodes
                        .kind(pattern)
                        .as_pattern()
                        .cloned()
                        .expect("node should be pattern")],
                );
                if !is_useful {
                    redundant.push((
                        PrettyPattern::from_pattern(nodes, pattern),
                        nodes.span(pattern),
                    ));
                }
                prev_rows.push((Vec::from([(pattern)]), body));
                (redundant, prev_rows)
            },
        )
        .0
}

pub fn is_useful(
    nodes: &NodeArena,
    type_ctors: &FxHashMap<String, FxHashMap<String, (usize, usize)>>,
    matrix: &[(Vec<Node>, Node)],
    query: &[Pattern],
) -> bool {
    if matrix.is_empty() {
        true
    } else if matrix.iter().any(|(r, _)| r.is_empty()) || query.is_empty() {
        false
    } else {
        let ctors = nodes
            .ty(matrix[0].0[0])
            .expect("pattern must have type")
            .as_adt()
            .and_then(|(n, _)| type_ctors.get(n))
            .cloned()
            .unwrap_or_default();
        match &query[0] {
            Pattern::Wildcard | Pattern::Identifier(..) => {
                if let (default, _) = default(nodes, matrix)
                    && !default.is_empty()
                {
                    is_useful(nodes, type_ctors, &default, &query[1..])
                } else {
                    let mut res = ctors.is_empty();
                    for (name, (_, arity)) in ctors {
                        let new_matrix = specialize(nodes, matrix, &name, arity);
                        let new_query = std::iter::repeat(Pattern::Wildcard)
                            .take(arity)
                            .chain(query[1..].iter().cloned())
                            .collect_vec();
                        if is_useful(nodes, type_ctors, &new_matrix, &new_query) {
                            res = true;
                            break;
                        }
                    }
                    res
                }
            }
            Pattern::Constructor(name, _) => {
                let (_, arity) = ctors.get(name).copied().unwrap_or_default();
                let new_matrix = specialize(nodes, matrix, &name, arity);
                let new_query = std::iter::repeat(Pattern::Wildcard)
                    .take(arity)
                    .chain(query[1..].iter().cloned())
                    .collect_vec();
                is_useful(nodes, type_ctors, &new_matrix, &new_query)
            }
        }
    }
}

pub fn default(
    nodes: &NodeArena,
    matrix: &[(Vec<Node>, Node)],
) -> (Vec<(Vec<Node>, Node)>, FxHashSet<String>) {
    matrix.iter().filter(|(row, _)| !row.is_empty()).fold(
        (Vec::new(), FxHashSet::default()),
        |(mut new_matrix, mut bindings), (row, body)| match nodes
            .kind(row[0])
            .as_pattern()
            .expect("node should be pattern")
        {
            Pattern::Wildcard => {
                new_matrix.push((row[1..].to_vec(), *body));
                (new_matrix, bindings)
            }
            Pattern::Identifier(id) => {
                new_matrix.push((row[1..].to_vec(), *body));
                bindings.insert(id.clone());
                (new_matrix, bindings)
            }
            _ => (new_matrix, bindings),
        },
    )
}

pub fn specialize(
    nodes: &NodeArena,
    matrix: &[(Vec<Node>, Node)],
    ctor: &str,
    arity: usize,
) -> Vec<(Vec<Node>, Node)> {
    matrix
        .iter()
        .filter(|(row, _)| !row.is_empty())
        .fold(Vec::new(), |mut acc, (row, body)| {
            match nodes
                .kind(row[0])
                .as_pattern()
                .expect("node should be pattern")
            {
                Pattern::Constructor(name, patterns) if name == ctor => {
                    acc.push((
                        patterns.iter().chain(row[1..].iter()).copied().collect(),
                        *body,
                    ));
                    acc
                }
                Pattern::Wildcard | Pattern::Identifier(_) => {
                    acc.push((
                        std::iter::repeat(row[0])
                            .take(arity)
                            .chain(row[1..].iter().copied())
                            .collect(),
                        *body,
                    ));
                    acc
                }
                _ => acc,
            }
        })
}
