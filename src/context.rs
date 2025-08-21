use core::fmt;
use std::fmt::Formatter;

use crate::{expr::Expr, span::Span, type_system::Type};

#[derive(Debug, Clone, Copy)]
pub struct NodeId(usize);

impl NodeId {
    pub fn display<'a>(&self, context: &'a Context) -> Display<'a> {
        Display::new(*self, context)
    }
}

pub struct Display<'a> {
    id: NodeId,
    context: &'a Context,
}

impl<'a> Display<'a> {
    pub fn new(id: NodeId, context: &'a Context) -> Self {
        Self { id, context }
    }
}

impl<'a> fmt::Display for Display<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.context.get_node(self.id) {
            Node::Expr(expr) => match expr {
                Expr::Unit => write!(f, "()"),
                Expr::Boolean(val) => write!(f, "{val}"),
                Expr::Integer(val) => write!(f, "{val}"),
                Expr::Identifier(id) => write!(f, "{id}"),
                Expr::Condition { cond, then, alt } => write!(
                    f,
                    "if {} then {} else {}",
                    Display::new(*cond, self.context),
                    Display::new(*then, self.context),
                    Display::new(*alt, self.context)
                ),
                Expr::Infix { lhs, op, rhs } => write!(
                    f,
                    "({} {} {})",
                    Display::new(*lhs, self.context),
                    op,
                    Display::new(*rhs, self.context)
                ),
                Expr::Prefix { op, rhs } => write!(f, "{}{}", op, Display::new(*rhs, self.context)),
                Expr::Lambda { param, body } => write!(
                    f,
                    "{} -> {}",
                    Display::new(*param, self.context),
                    Display::new(*body, self.context)
                ),
                Expr::Application { func, arg } => write!(
                    f,
                    "({} {})",
                    Display::new(*func, self.context),
                    Display::new(*arg, self.context)
                ),
                Expr::Nil => write!(f, "[]"),
                Expr::Cons { head, tail } => {
                    let mut tail = *tail;
                    let mut elems = vec![*head];
                    loop {
                        match self.context.get_node(tail) {
                            Node::Expr(Expr::Cons { head, tail: t }) => {
                                elems.push(*head);
                                tail = *t;
                            }
                            Node::Expr(Expr::Nil) => {
                                write!(f, "[")?;
                                write!(
                                    f,
                                    "{}",
                                    elems
                                        .iter()
                                        .map(|elem| Display::new(*elem, self.context).to_string())
                                        .collect::<Vec<String>>()
                                        .join(", ")
                                )?;
                                return write!(f, "]");
                            }
                            _ => unreachable!(),
                        }
                    }
                }
            },
            Node::Bind(name, expr) => {
                write!(f, "{} = {}", name, Display::new(*expr, self.context))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Node {
    Expr(Expr),
    Bind(String, NodeId),
}

#[derive(Debug)]
pub struct Context {
    nodes: Vec<Node>,
    spans: Vec<Span>,
    types: Vec<Type>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            spans: Vec::new(),
            types: Vec::new(),
        }
    }

    pub fn add(&mut self, node: Node) -> NodeId {
        let id = NodeId(self.nodes.len());
        self.nodes.push(node);
        self.spans.push(Span::default());
        self.types.push(Type::Unit);
        id
    }

    pub fn add_expr(&mut self, expr: Expr) -> NodeId {
        self.add(Node::Expr(expr))
    }

    pub fn add_bind(&mut self, name: &str, expr: NodeId) -> NodeId {
        self.add(Node::Bind(name.to_string(), expr))
    }

    pub fn set_span(&mut self, id: NodeId, span: Span) {
        self.spans[id.0] = span;
    }

    pub fn set_type(&mut self, id: NodeId, ty: Type) {
        self.types[id.0] = ty;
    }

    pub fn get_node(&self, id: NodeId) -> &Node {
        &self.nodes[id.0]
    }

    pub fn get_span(&self, id: NodeId) -> &Span {
        &self.spans[id.0]
    }

    pub fn get_type(&self, id: NodeId) -> &Type {
        &self.types[id.0]
    }
}
