use crate::ast::node::Node;

#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    Char(char),
    Integer(i64),
    Constructor(String),
    Identifier(String),
    Match(Node, Vec<(Node, Node)>),
    Block(Vec<Node>),
    Lambda(String, Node),
    Application(Node, Node),
}
