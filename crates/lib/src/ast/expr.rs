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

impl Expr {
    pub const fn is_unit(&self) -> bool {
        matches!(self, Expr::Unit)
    }

    pub const fn is_char(&self) -> bool {
        matches!(self, Expr::Char(..))
    }

    pub const fn is_integer(&self) -> bool {
        matches!(self, Expr::Integer(..))
    }

    pub const fn is_constructor(&self) -> bool {
        matches!(self, Expr::Constructor(..))
    }

    pub const fn is_identifier(&self) -> bool {
        matches!(self, Expr::Identifier(..))
    }

    pub const fn is_match(&self) -> bool {
        matches!(self, Expr::Match(..))
    }

    pub const fn is_block(&self) -> bool {
        matches!(self, Expr::Block(..))
    }

    pub const fn is_lambda(&self) -> bool {
        matches!(self, Expr::Lambda(..))
    }

    pub const fn is_application(&self) -> bool {
        matches!(self, Expr::Application(..))
    }

    pub fn as_char(&self) -> Option<char> {
        match self {
            Expr::Char(c) => Some(*c),
            _ => None,
        }
    }

    pub fn as_integer(&self) -> Option<i64> {
        match self {
            Expr::Integer(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_constructor(&self) -> Option<&str> {
        match self {
            Expr::Constructor(name) => Some(name.as_str()),
            _ => None,
        }
    }

    pub fn as_identifier(&self) -> Option<&str> {
        match self {
            Expr::Identifier(id) => Some(id.as_str()),
            _ => None,
        }
    }

    pub fn as_match(&self) -> Option<(&Node, &[(Node, Node)])> {
        match self {
            Expr::Match(scrutinee, arms) => Some((scrutinee, arms.as_slice())),
            _ => None,
        }
    }

    pub fn as_match_mut(&mut self) -> Option<(&mut Node, &mut [(Node, Node)])> {
        match self {
            Expr::Match(scrutinee, arms) => Some((scrutinee, arms.as_mut_slice())),
            _ => None,
        }
    }

    pub fn as_block(&self) -> Option<&[Node]> {
        match self {
            Expr::Block(nodes) => Some(nodes.as_slice()),
            _ => None,
        }
    }

    pub fn as_block_mut(&mut self) -> Option<&mut [Node]> {
        match self {
            Expr::Block(nodes) => Some(nodes.as_mut_slice()),
            _ => None,
        }
    }

    pub fn as_lambda(&self) -> Option<(&str, &Node)> {
        match self {
            Expr::Lambda(l, r) => Some((l.as_str(), r)),
            _ => None,
        }
    }

    pub fn as_lambda_mut(&mut self) -> Option<(&mut String, &mut Node)> {
        match self {
            Expr::Lambda(l, r) => Some((l, r)),
            _ => None,
        }
    }

    pub fn as_application(&self) -> Option<(&Node, &Node)> {
        match self {
            Expr::Application(l, r) => Some((l, r)),
            _ => None,
        }
    }

    pub fn as_application_mut(&mut self) -> Option<(&mut Node, &mut Node)> {
        match self {
            Expr::Application(l, r) => Some((l, r)),
            _ => None,
        }
    }
}
