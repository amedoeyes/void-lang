use core::fmt;
use std::fmt::Formatter;

use itertools::Itertools;

use crate::{
    expr::{Expr, TypeExpr},
    span::Span,
    type_system::Type,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct NodeId(pub usize);

#[derive(Debug, Clone)]
pub enum Node {
    Module(Vec<NodeId>),
    TypeExpr(TypeExpr),
    Expr(Expr),
    Type(String, Vec<String>, Vec<(String, Vec<NodeId>)>),
    Primitive(String, NodeId, String),
    Bind(String, Option<NodeId>, NodeId),
}

pub struct Display<'a> {
    id: NodeId,
    context: &'a Context,
}

#[derive(Debug, Clone)]
pub struct Context {
    nodes: Vec<Node>,
    spans: Vec<Span>,
    types: Vec<Option<Type>>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            spans: Vec::new(),
            types: Vec::new(),
        }
    }

    pub fn nodes(&self) -> &[Node] {
        &self.nodes
    }

    pub fn add(&mut self, node: Node) -> NodeId {
        let id = NodeId(self.nodes.len());
        self.nodes.push(node);
        self.spans.push(Span::default());
        self.types.push(None);
        id
    }

    pub fn add_module(&mut self, nodes: &[NodeId]) -> NodeId {
        self.add(Node::Module(nodes.into()))
    }

    pub fn add_type_expr(&mut self, expr: TypeExpr) -> NodeId {
        self.add(Node::TypeExpr(expr))
    }

    pub fn add_expr(&mut self, expr: Expr) -> NodeId {
        self.add(Node::Expr(expr))
    }

    pub fn add_type(
        &mut self,
        name: String,
        params: Vec<String>,
        constructors: Vec<(String, Vec<NodeId>)>,
    ) -> NodeId {
        self.add(Node::Type(name, params, constructors))
    }

    pub fn add_primitive(&mut self, name: &str, type_expr: NodeId, link_name: &str) -> NodeId {
        self.add(Node::Primitive(name.into(), type_expr, link_name.into()))
    }

    pub fn add_bind(&mut self, name: &str, type_expr: Option<NodeId>, expr: NodeId) -> NodeId {
        self.add(Node::Bind(name.to_string(), type_expr, expr))
    }

    pub fn set_span(&mut self, id: NodeId, span: Span) {
        self.spans[id.0] = span;
    }

    pub fn set_type(&mut self, id: NodeId, ty: Type) {
        self.types[id.0] = Some(ty);
    }

    pub fn get_node(&self, id: NodeId) -> &Node {
        &self.nodes[id.0]
    }

    pub fn get_node_mut(&mut self, id: NodeId) -> &mut Node {
        &mut self.nodes[id.0]
    }

    pub fn get_type_expr(&self, id: NodeId) -> Option<&TypeExpr> {
        self.nodes.get(id.0).and_then(|n| {
            if let Node::TypeExpr(expr) = n {
                Some(expr)
            } else {
                None
            }
        })
    }

    pub fn get_type_expr_mut(&mut self, id: NodeId) -> Option<&mut TypeExpr> {
        self.nodes.get_mut(id.0).and_then(|n| {
            if let Node::TypeExpr(expr) = n {
                Some(expr)
            } else {
                None
            }
        })
    }

    pub fn get_expr(&self, id: NodeId) -> Option<&Expr> {
        self.nodes.get(id.0).and_then(|n| {
            if let Node::Expr(expr) = n {
                Some(expr)
            } else {
                None
            }
        })
    }

    pub fn get_expr_mut(&mut self, id: NodeId) -> Option<&mut Expr> {
        self.nodes.get_mut(id.0).and_then(|n| {
            if let Node::Expr(expr) = n {
                Some(expr)
            } else {
                None
            }
        })
    }

    pub fn get_span(&self, id: NodeId) -> Span {
        self.spans[id.0]
    }

    pub fn get_type(&self, id: NodeId) -> &Option<Type> {
        &self.types[id.0]
    }
}

impl NodeId {
    pub fn display<'a>(&self, context: &'a Context) -> Display<'a> {
        Display::new(*self, context)
    }
}

impl<'a> Display<'a> {
    pub fn new(id: NodeId, context: &'a Context) -> Self {
        Self { id, context }
    }
}

impl<'a> fmt::Display for Display<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.context.get_node(self.id) {
            Node::Module(nodes) => write!(
                f,
                "{}",
                nodes
                    .iter()
                    .map(|n| Display::new(*n, self.context))
                    .join(" ")
            ),
            Node::TypeExpr(expr) => match expr {
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
            Node::Expr(expr) => match expr {
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
                            .map(|(p, b)| format!("{} => {}", p, Display::new(*b, self.context)))
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
                Expr::Lambda { param, body } => {
                    write!(f, "{} -> {}", param, Display::new(*body, self.context))
                }
                Expr::Application { func, arg } => write!(
                    f,
                    "({} {})",
                    Display::new(*func, self.context),
                    Display::new(*arg, self.context)
                ),
            },
            Node::Type(name, params, constructors) => {
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
            Node::Bind(name, type_expr, expr) => {
                write!(f, "let {}", name)?;
                if let Some(type_expr) = type_expr {
                    write!(f, " : {}", Display::new(*type_expr, self.context))?;
                }
                write!(f, " = {};", Display::new(*expr, self.context))?;
                Ok(())
            }
            Node::Primitive(name, type_expr, link_name) => {
                write!(
                    f,
                    "primitive {} : {} = {};",
                    name,
                    Display::new(*type_expr, self.context),
                    link_name
                )
            }
        }
    }
}
