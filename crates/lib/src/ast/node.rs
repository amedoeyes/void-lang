use std::fmt::{self, Formatter};

use itertools::Itertools;

use crate::ast::{arena::NodeArena, expr::Expr, pattern::Pattern, type_expr::TypeExpr};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Node(pub usize);

pub struct Display<'a> {
    node: Node,
    context: &'a NodeArena,
}

impl Node {
    pub fn display<'a>(&self, context: &'a NodeArena) -> Display<'a> {
        Display::new(*self, context)
    }
}

impl<'a> Display<'a> {
    pub fn new(node: Node, context: &'a NodeArena) -> Self {
        Self { node, context }
    }
}

impl<'a> fmt::Display for Display<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.context.kind(self.node) {
            NodeKind::Module(nodes) => write!(
                f,
                "{}",
                nodes
                    .iter()
                    .map(|n| Display::new(*n, self.context))
                    .join(" ")
            ),
            NodeKind::TypeExpr(expr) => match expr {
                TypeExpr::Unit => write!(f, "()"),
                TypeExpr::Identifier(id) => write!(f, "{id}"),
                TypeExpr::Constructor(name, args) => {
                    write!(f, "{}", name)?;
                    if !args.is_empty() {
                        write!(
                            f,
                            "<{}>",
                            args.iter()
                                .map(|a| Display::new(*a, self.context))
                                .join(", "),
                        )?;
                    }
                    Ok(())
                }
                TypeExpr::Lambda(l, r) => {
                    match self.context.get_type_expr(*l).expect("type expr") {
                        TypeExpr::Lambda(..) => write!(f, "({})", Display::new(*l, self.context))?,
                        _ => write!(f, "{}", Display::new(*l, self.context))?,
                    }
                    write!(f, " -> {}", Display::new(*r, self.context))
                }
                TypeExpr::Forall(params, body) => {
                    write!(
                        f,
                        "<{}> {}",
                        params.iter().join(", "),
                        Display::new(*body, self.context)
                    )
                }
            },
            NodeKind::Expr(expr) => match expr {
                Expr::Unit => write!(f, "()"),
                Expr::Char(val) => write!(f, "'{}'", val.escape_default()),
                Expr::Integer(val) => write!(f, "{val}"),
                Expr::Constructor(cons) => write!(f, "{cons}"),
                Expr::Identifier(id) => write!(f, "{id}"),
                Expr::Match(scrutinee, branches) => {
                    write!(
                        f,
                        "match {} with {{ {} }}",
                        Display::new(*scrutinee, self.context),
                        branches
                            .iter()
                            .map(|(p, b)| format!(
                                "{} => {}",
                                Display::new(*p, self.context),
                                Display::new(*b, self.context)
                            ))
                            .join(", ")
                    )
                }
                Expr::Block(nodes) => write!(
                    f,
                    "{{ {} }}",
                    nodes
                        .iter()
                        .map(|n| Display::new(*n, self.context))
                        .join(" ")
                ),
                Expr::Lambda(l, r) => {
                    write!(f, "{} -> {}", l, Display::new(*r, self.context))
                }
                Expr::Application(l, r) => write!(
                    f,
                    "({} {})",
                    Display::new(*l, self.context),
                    Display::new(*r, self.context)
                ),
            },
            NodeKind::Type(name, params, constructors) => {
                write!(f, "type {} = ", name)?;
                if !params.is_empty() {
                    write!(f, "<{}> ", params.join(", "))?;
                }
                write!(f, "{{ ")?;
                write!(
                    f,
                    "{}",
                    constructors
                        .iter()
                        .map(|(c, a)| format!(
                            "{}{}{}",
                            c,
                            if !a.is_empty() { " " } else { "" },
                            a.iter()
                                .map(|a| Display::new(*a, self.context).to_string())
                                .join(" ")
                        ))
                        .collect::<Vec<String>>()
                        .join(", ")
                )?;
                write!(f, " }};")?;
                Ok(())
            }
            NodeKind::Bind(name, type_expr, expr) => {
                write!(f, "let {}", name)?;
                if let Some(type_expr) = type_expr {
                    write!(f, " : {}", Display::new(*type_expr, self.context))?;
                }
                write!(f, " = {};", Display::new(*expr, self.context))?;
                Ok(())
            }
            NodeKind::Primitive(name, type_expr, link_name) => {
                write!(
                    f,
                    "primitive {} : {} = {};",
                    name,
                    Display::new(*type_expr, self.context),
                    link_name
                )
            }
            NodeKind::Pattern(pattern) => match pattern {
                Pattern::Wildcard => write!(f, "_"),
                Pattern::Identifier(id) => write!(f, "{id}"),
                Pattern::Constructor(name, patterns) => {
                    write!(
                        f,
                        "{}{}{}",
                        name,
                        if !patterns.is_empty() { " " } else { "" },
                        patterns
                            .iter()
                            .map(|p| format!("{}", Display::new(*p, self.context)))
                            .join(" ")
                    )
                }
            },
        }
    }
}

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
}
