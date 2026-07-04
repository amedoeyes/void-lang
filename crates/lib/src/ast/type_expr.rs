use crate::ast::node::Node;

#[derive(Debug, Clone)]
pub enum TypeExpr {
    Unit,
    Identifier(String),
    Constructor(String, Vec<Node>),
    Lambda(Node, Node),
    Forall(Vec<String>, Node),
}
