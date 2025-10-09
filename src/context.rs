use core::fmt;
use std::{cell::RefCell, fmt::Formatter, rc::Rc};

use fxhash::FxHashMap;

use crate::{
    eval::{self, List, Value},
    expr::Expr,
    span::Span,
    type_system::Type,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct NodeId(usize);

impl NodeId {
    pub fn display<'a>(&self, context: &'a Context) -> Display<'a> {
        Display::new(*self, context)
    }
}

#[derive(Debug, Clone)]
pub enum Node {
    Expr(Expr),
    Bind(String, NodeId),
}

pub type BuiltinEvalFn = fn(&Context, Rc<RefCell<Value>>, Span) -> eval::Result<Rc<RefCell<Value>>>;

#[derive(Debug, Clone)]
pub enum BuiltinKind {
    Function(BuiltinEvalFn),
    Value(Rc<RefCell<Value>>),
}

#[derive(Debug, Clone)]
pub struct Builtin {
    pub ty: Type,
    pub kind: BuiltinKind,
}

#[derive(Debug, Clone)]
pub struct Context {
    nodes: Vec<Node>,
    spans: Vec<Span>,
    types: Vec<Type>,
    builtins: FxHashMap<String, Builtin>,
}

impl Context {
    pub fn new() -> Self {
        let mut ctx = Self {
            nodes: Vec::new(),
            spans: Vec::new(),
            types: Vec::new(),
            builtins: FxHashMap::default(),
        };

        ctx.add_builtins();

        ctx
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

    pub fn builtins(&self) -> &FxHashMap<String, Builtin> {
        &self.builtins
    }

    fn add_builtins(&mut self) {
        self.builtins.insert(
            "args".into(),
            Builtin {
                ty: Type::List(Box::new(Type::List(Box::new(Type::Char)))),
                kind: BuiltinKind::Value({
                    let args = std::env::args().skip(2).collect::<Vec<_>>();
                    let mut arg_list = List::Nil;

                    for arg in args.iter().rev() {
                        let mut string_list = List::Nil;
                        for ch in arg.chars().rev() {
                            string_list = string_list.push(Value::Char(ch));
                        }
                        arg_list = arg_list.push(Value::List(string_list));
                    }

                    Rc::new(RefCell::new(Value::List(arg_list)))
                }),
            },
        );

        self.builtins.insert(
            "read".into(),
            Builtin {
                ty: Type::Fun(
                    Box::new(Type::List(Box::new(Type::Char))),
                    Box::new(Type::List(Box::new(Type::Char))),
                ),
                kind: BuiltinKind::Function(|ctx, arg, span| {
                    let path = match &*arg.borrow() {
                        Value::List(l) => {
                            let mut list = l.clone();
                            let mut chars = Vec::new();
                            while let Some(mut h) = list.head() {
                                match h.force(ctx).unwrap() {
                                    Value::Char(ch) => chars.push(ch),
                                    _ => unreachable!(),
                                }
                                list = list.tail().unwrap_or_default();
                            }
                            chars.into_iter().collect::<String>()
                        }
                        _ => unreachable!(),
                    };

                    let contents = if path == "-" {
                        use std::io::Read;
                        let mut input = String::new();
                        std::io::stdin().read_to_string(&mut input).map_err(|e| {
                            eval::Error::IOError(format!("Failed to read from stdin: {}", e), span)
                        })?;
                        input
                    } else {
                        std::fs::read_to_string(&path).map_err(|e| {
                            eval::Error::IOError(
                                format!("Failed to read file '{}': {}", path, e),
                                span,
                            )
                        })?
                    };

                    let mut list = List::default();
                    for ch in contents.chars().rev() {
                        list = list.push(Value::Char(ch))
                    }

                    Ok(Rc::new(RefCell::new(Value::List(list))))
                }),
            },
        );

        self.builtins.insert(
            "print".into(),
            Builtin {
                ty: Type::Fun(Box::new(Type::Var(1)), Box::new(Type::Var(1))),
                kind: BuiltinKind::Function(|ctx, arg, _| {
                    println!("{}", arg.borrow().display(ctx));
                    Ok(arg)
                }),
            },
        );

        self.builtins.insert(
            "head".into(),
            Builtin {
                ty: Type::Fun(
                    Box::new(Type::List(Box::new(Type::Var(2)))),
                    Box::new(Type::Var(2)),
                ),
                kind: BuiltinKind::Function(|ctx, arg, span| match &*arg.borrow() {
                    Value::List(l) => Ok(Rc::new(RefCell::new(
                        l.head().ok_or(eval::Error::EmptyList(span))?.force(ctx)?,
                    ))),
                    _ => unreachable!(),
                }),
            },
        );

        self.builtins.insert(
            "tail".into(),
            Builtin {
                ty: Type::Fun(
                    Box::new(Type::List(Box::new(Type::Var(3)))),
                    Box::new(Type::List(Box::new(Type::Var(3)))),
                ),
                kind: BuiltinKind::Function(|_, arg, span| match &*arg.borrow() {
                    Value::List(l) => Ok(Rc::new(RefCell::new(Value::List(
                        l.tail().ok_or(eval::Error::EmptyList(span))?,
                    )))),
                    _ => unreachable!(),
                }),
            },
        );
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
                Expr::Char(val) => write!(f, "'{}'", val.escape_default()),
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
