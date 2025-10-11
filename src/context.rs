use core::fmt;
use std::{cmp::Ordering, fmt::Formatter};

use fxhash::{FxHashMap, FxHashSet};

use crate::{
    eval::{self, List, Value},
    expr::Expr,
    span::Span,
    ty,
    type_system::{Constraint, Type, TypeClass},
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct NodeId(usize);

#[derive(Debug, Clone)]
pub enum Node {
    Expr(Expr),
    Bind(String, NodeId),
}

pub struct Display<'a> {
    id: NodeId,
    context: &'a Context,
}

#[derive(Debug, Clone)]
pub enum BuiltinKind {
    Value(Box<Value>),
    Function(fn(&Context, &Value, Span) -> eval::Result<Value>),
    Operator(fn(&Context, (&Value, Span), (&Value, Span)) -> eval::Result<Value>),
}

#[derive(Debug, Clone)]
pub struct Builtin {
    pub ty: Type,
    pub kind: BuiltinKind,
}

#[derive(Debug, Clone, Copy)]
pub enum Associativity {
    Left,
    Right,
    None,
}

#[derive(Debug, Clone, Copy)]
pub struct Operator {
    pub precedence: i32,
    pub associativity: Associativity,
}

impl Default for Operator {
    fn default() -> Self {
        Self {
            precedence: 9,
            associativity: Associativity::Left,
        }
    }
}

impl Operator {
    pub fn binding_power(&self) -> (i32, i32) {
        match self.associativity {
            Associativity::Left => (self.precedence, self.precedence + 1),
            Associativity::Right => (self.precedence + 1, self.precedence),
            Associativity::None => (self.precedence, self.precedence),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Context {
    nodes: Vec<Node>,
    spans: Vec<Span>,
    types: Vec<Type>,
    builtins: FxHashMap<String, Builtin>,
    operators: FxHashMap<String, Operator>,
}

impl Context {
    pub fn new() -> Self {
        let mut ctx = Self {
            nodes: Vec::new(),
            spans: Vec::new(),
            types: Vec::new(),
            operators: FxHashMap::default(),
            builtins: FxHashMap::default(),
        };

        ctx.add_operators();
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

    pub fn get_span(&self, id: NodeId) -> Span {
        self.spans[id.0]
    }

    pub fn get_type(&self, id: NodeId) -> &Type {
        &self.types[id.0]
    }

    pub fn add_operator(&mut self, symbol: &str, precedence: i32, associativity: Associativity) {
        self.operators.insert(
            symbol.to_string(),
            Operator {
                precedence,
                associativity,
            },
        );
    }

    pub fn get_operator(&self, symbol: &str) -> Option<&Operator> {
        self.operators.get(symbol)
    }

    pub fn add_builtin(&mut self, symbol: &str, ty: Type, kind: BuiltinKind) {
        self.builtins
            .insert(symbol.to_string(), Builtin { ty, kind });
    }

    pub fn builtins(&self) -> &FxHashMap<String, Builtin> {
        &self.builtins
    }

    fn add_builtins(&mut self) {
        pub fn cmp(ctx: &Context, a: &Value, b: &Value) -> eval::Result<Ordering> {
            match (a, b) {
                (Value::Integer(a), Value::Integer(b)) => Ok(a.cmp(b)),
                (Value::Boolean(a), Value::Boolean(b)) => Ok(a.cmp(b)),
                (Value::Char(a), Value::Char(b)) => Ok(a.cmp(b)),
                (Value::List(a), Value::List(b)) => {
                    let mut l1 = a.clone();
                    let mut l2 = b.clone();
                    loop {
                        match (l1.head(), l2.head()) {
                            (None, None) => return Ok(Ordering::Equal),
                            (None, Some(_)) => return Ok(Ordering::Less),
                            (Some(_), None) => return Ok(Ordering::Greater),
                            (Some(mut a), Some(mut b)) => {
                                match cmp(ctx, &a.force(ctx)?, &b.force(ctx)?)? {
                                    Ordering::Equal => {
                                        l1 = l1.tail().unwrap_or_default();
                                        l2 = l2.tail().unwrap_or_default();
                                    }
                                    ordering => return Ok(ordering),
                                }
                            }
                        }
                    }
                }
                (Value::Unit, Value::Unit) => Ok(Ordering::Equal),
                _ => unreachable!(),
            }
        }

        self.add_builtin(
            "||",
            ty!(Bool -> Bool -> Bool),
            BuiltinKind::Operator(|_, (a, _), (b, _)| match (a, b) {
                (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(*a || *b)),
                _ => unreachable!(),
            }),
        );

        self.add_builtin(
            "&&",
            ty!(Bool -> Bool -> Bool),
            BuiltinKind::Operator(|_, (a, _), (b, _)| match (a, b) {
                (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(*a && *b)),
                _ => unreachable!(),
            }),
        );

        self.add_builtin(
            "==",
            ty!(forall 0 . (Eq 0) => 0 -> 0 -> Bool),
            BuiltinKind::Operator(|ctx, (a, _), (b, _)| {
                Ok(Value::Boolean(cmp(ctx, a, b)? == Ordering::Equal))
            }),
        );

        self.add_builtin(
            "!=",
            ty!(forall 0 . (Eq 0) => 0 -> 0 -> Bool),
            BuiltinKind::Operator(|ctx, (a, _), (b, _)| {
                Ok(Value::Boolean(cmp(ctx, a, b)? != Ordering::Equal))
            }),
        );

        self.add_builtin(
            "<",
            ty!(forall 0 . (Ord 0) => 0 -> 0 -> Bool),
            BuiltinKind::Operator(|ctx, (a, _), (b, _)| {
                Ok(Value::Boolean(cmp(ctx, a, b)? == Ordering::Less))
            }),
        );

        self.add_builtin(
            "<=",
            ty!(forall 0 . (Ord 0) => 0 -> 0 -> Bool),
            BuiltinKind::Operator(|ctx, (a, _), (b, _)| {
                Ok(Value::Boolean(matches!(
                    cmp(ctx, a, b)?,
                    Ordering::Less | Ordering::Equal
                )))
            }),
        );

        self.add_builtin(
            ">",
            ty!(forall 0 . (Ord 0) => 0 -> 0 -> Bool),
            BuiltinKind::Operator(|ctx, (a, _), (b, _)| {
                Ok(Value::Boolean(cmp(ctx, a, b)? == Ordering::Greater))
            }),
        );

        self.add_builtin(
            ">=",
            ty!(forall 0 . (Ord 0) => 0 -> 0 -> Bool),
            BuiltinKind::Operator(|ctx, (a, _), (b, _)| {
                Ok(Value::Boolean(matches!(
                    cmp(ctx, a, b)?,
                    Ordering::Greater | Ordering::Equal
                )))
            }),
        );

        self.add_builtin(
            "++",
            ty!(forall 0 . [0] -> [0] -> [0]),
            BuiltinKind::Operator(|_, (a, _), (b, _)| match (a, b) {
                (Value::List(a), Value::List(b)) => Ok(Value::List(a.clone().append(b.clone()))),
                _ => unreachable!(),
            }),
        );

        self.add_builtin(
            ":",
            ty!(forall 0 . 0 -> [0] -> [0]),
            BuiltinKind::Operator(|_, (a, _), (b, _)| match (a, b) {
                (Value::List(a), Value::List(b)) => {
                    Ok(Value::List(b.clone().push(Value::List(a.clone()))))
                }
                (a, Value::List(b)) => Ok(Value::List(b.clone().push(a.clone()))),
                _ => unreachable!(),
            }),
        );

        self.add_builtin(
            "+",
            ty!(forall 0 . (Num 0) => 0 -> 0 -> 0),
            BuiltinKind::Operator(|_, (a, _), (b, _)| match (a, b) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
                _ => unreachable!(),
            }),
        );

        self.add_builtin(
            "-",
            ty!(forall 0 . (Num 0) => 0 -> 0 -> 0),
            BuiltinKind::Operator(|_, (a, _), (b, _)| match (a, b) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a - b)),
                _ => unreachable!(),
            }),
        );

        self.add_builtin(
            "*",
            ty!(forall 0 . (Num 0) => 0 -> 0 -> 0),
            BuiltinKind::Operator(|_, (a, _), (b, _)| match (a, b) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
                _ => unreachable!(),
            }),
        );

        self.add_builtin(
            "/",
            ty!(forall 0 . (Num 0) => 0 -> 0 -> 0),
            BuiltinKind::Operator(|_, (a, _), (b, span)| match (a, b) {
                (Value::Integer(a), Value::Integer(b)) => a
                    .checked_div(*b)
                    .map(Value::Integer)
                    .ok_or(eval::Error::DivisionByZero(span)),
                _ => unreachable!(),
            }),
        );

        self.add_builtin(
            "%",
            ty!(forall 0 . (Num 0) => 0 -> 0 -> 0),
            BuiltinKind::Operator(|_, (a, _), (b, _)| match (a, b) {
                (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a % b)),
                _ => unreachable!(),
            }),
        );

        self.add_builtin(
            "args",
            ty!([[Char]]),
            BuiltinKind::Value({
                let args = std::env::args().skip(2).collect::<Vec<_>>();
                let mut arg_list = List::Nil;

                for arg in args.iter().rev() {
                    let mut string_list = List::Nil;
                    for ch in arg.chars().rev() {
                        string_list = string_list.push(Value::Char(ch));
                    }
                    arg_list = arg_list.push(Value::List(string_list));
                }

                Box::new(Value::List(arg_list))
            }),
        );

        self.add_builtin(
            "not",
            ty!(Bool -> Bool),
            BuiltinKind::Function(|_, arg, _| match arg {
                Value::Boolean(b) => Ok(Value::Boolean(!b)),
                _ => unreachable!(),
            }),
        );

        self.add_builtin(
            "read",
            ty!([Char] -> [Char]),
            BuiltinKind::Function(|ctx, arg, span| {
                let path = match arg {
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
                        eval::Error::IO(format!("Failed to read from stdin: {}", e), span)
                    })?;
                    input
                } else {
                    std::fs::read_to_string(&path).map_err(|e| {
                        eval::Error::IO(format!("Failed to read file '{}': {}", path, e), span)
                    })?
                };

                let mut list = List::default();
                for ch in contents.chars().rev() {
                    list = list.push(Value::Char(ch))
                }

                Ok(Value::List(list))
            }),
        );

        self.add_builtin(
            "print",
            ty!(forall 0 . 0 -> 0),
            BuiltinKind::Function(|ctx, arg, _| {
                println!("{}", arg.display(ctx));
                Ok(arg.clone())
            }),
        );

        self.add_builtin(
            "head",
            ty!(forall 0 . [0] -> 0),
            BuiltinKind::Function(|ctx, arg, span| match arg {
                Value::List(l) => Ok(l.head().ok_or(eval::Error::EmptyList(span))?.force(ctx)?),
                _ => unreachable!(),
            }),
        );

        self.add_builtin(
            "tail",
            ty!(forall 0 . [0] -> [0]),
            BuiltinKind::Function(|_, arg, span| match arg {
                Value::List(l) => Ok(Value::List(l.tail().ok_or(eval::Error::EmptyList(span))?)),
                _ => unreachable!(),
            }),
        );
    }

    fn add_operators(&mut self) {
        self.add_operator("||", 1, Associativity::Right);
        self.add_operator("&&", 2, Associativity::Right);
        self.add_operator("==", 3, Associativity::None);
        self.add_operator("!=", 3, Associativity::None);
        self.add_operator("<", 3, Associativity::None);
        self.add_operator("<=", 3, Associativity::None);
        self.add_operator(">", 3, Associativity::None);
        self.add_operator(">=", 3, Associativity::None);
        self.add_operator("++", 4, Associativity::Right);
        self.add_operator(":", 4, Associativity::Right);
        self.add_operator("+", 5, Associativity::Left);
        self.add_operator("-", 5, Associativity::Left);
        self.add_operator("*", 6, Associativity::Left);
        self.add_operator("/", 6, Associativity::Left);
        self.add_operator("%", 6, Associativity::Left);
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
                Expr::Infix { lhs, op, rhs } => {
                    write!(
                        f,
                        "({} {} {})",
                        Display::new(*lhs, self.context),
                        op,
                        Display::new(*rhs, self.context)
                    )
                }
                Expr::Lambda { param, body } => {
                    write!(f, "{} -> {}", param, Display::new(*body, self.context))
                }
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
