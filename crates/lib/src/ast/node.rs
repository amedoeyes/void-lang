use crate::ast::{expr::Expr, pattern::Pattern, type_expr::TypeExpr};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Node(pub usize);

#[derive(Debug, Clone)]
pub enum NodeKind {
    Module(Vec<Node>),
    TypeExpr(TypeExpr),
    Expr(Expr),
    Type(String, Vec<String>, Vec<(String, Vec<Node>)>),
    Pattern(Pattern),
    Primitive(String, Node, String),
    Bind(String, Option<Node>, Node),
}

impl NodeKind {
    pub fn is_module(&self) -> bool {
        matches!(self, NodeKind::Module(..))
    }
    pub fn is_type_expr(&self) -> bool {
        matches!(self, NodeKind::TypeExpr(..))
    }
    pub fn is_expr(&self) -> bool {
        matches!(self, NodeKind::Expr(..))
    }
    pub fn is_type(&self) -> bool {
        matches!(self, NodeKind::Type(..))
    }
    pub fn is_primitive(&self) -> bool {
        matches!(self, NodeKind::Primitive(..))
    }
    pub fn is_bind(&self) -> bool {
        matches!(self, NodeKind::Bind(..))
    }

    pub fn is_pattern(&self) -> bool {
        matches!(self, NodeKind::Pattern(..))
    }

    pub fn as_module(&self) -> Option<&[Node]> {
        match self {
            NodeKind::Module(nodes) => Some(&nodes),
            _ => None,
        }
    }

    pub fn as_type_expr(&self) -> Option<&TypeExpr> {
        match self {
            NodeKind::TypeExpr(type_expr) => Some(type_expr),
            _ => None,
        }
    }

    pub fn as_expr(&self) -> Option<&Expr> {
        match self {
            NodeKind::Expr(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_type(&self) -> Option<(&str, &[String], &[(String, Vec<Node>)])> {
        match self {
            NodeKind::Type(name, params, constructors) => {
                Some((name.as_str(), params.as_slice(), constructors.as_slice()))
            }
            _ => None,
        }
    }

    pub fn as_primitive(&self) -> Option<(&str, Node, &str)> {
        match self {
            Self::Primitive(name, type_expr, link_name) => {
                Some((name.as_str(), *type_expr, link_name.as_str()))
            }
            _ => None,
        }
    }

    pub fn as_bind(&self) -> Option<(&str, Option<Node>, Node)> {
        match self {
            Self::Bind(name, type_expr, expr) => Some((name.as_str(), *type_expr, *expr)),
            _ => None,
        }
    }

    pub fn as_pattern(&self) -> Option<&Pattern> {
        match self {
            Self::Pattern(pattern) => Some(pattern),
            _ => None,
        }
    }

    pub fn as_mut_module(&mut self) -> Option<&mut [Node]> {
        match self {
            NodeKind::Module(nodes) => Some(nodes.as_mut_slice()),
            _ => None,
        }
    }

    pub fn as_mut_type_expr(&mut self) -> Option<&mut TypeExpr> {
        match self {
            NodeKind::TypeExpr(type_expr) => Some(type_expr),
            _ => None,
        }
    }

    pub fn as_mut_expr(&mut self) -> Option<&mut Expr> {
        match self {
            NodeKind::Expr(expr) => Some(expr),
            _ => None,
        }
    }

    pub fn as_mut_type(
        &mut self,
    ) -> Option<(&mut String, &mut Vec<String>, &mut Vec<(String, Vec<Node>)>)> {
        match self {
            NodeKind::Type(name, params, constructors) => Some((name, params, constructors)),
            _ => None,
        }
    }

    pub fn as_mut_primitive(&mut self) -> Option<(&mut String, &mut Node, &mut String)> {
        match self {
            NodeKind::Primitive(name, type_expr, link_name) => Some((name, type_expr, link_name)),
            _ => None,
        }
    }

    pub fn as_mut_bind(&mut self) -> Option<(&mut String, &mut Option<Node>, &mut Node)> {
        match self {
            NodeKind::Bind(name, type_expr, expr) => Some((name, type_expr, expr)),
            _ => None,
        }
    }

    pub fn as_mut_pattern(&mut self) -> Option<&mut Pattern> {
        match self {
            NodeKind::Pattern(pattern) => Some(pattern),
            _ => None,
        }
    }
}
