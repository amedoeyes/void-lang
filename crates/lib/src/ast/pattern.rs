use crate::ast::node::Node;

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Identifier(String),
    Constructor(String, Vec<Node>),
}
