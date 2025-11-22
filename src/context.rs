use core::fmt;
use std::{collections::VecDeque, fmt::Formatter};

use fxhash::{FxHashMap, FxHashSet};

use crate::{
    eval::{self, Frame, List, Op, SharedValue, Value},
    expr::Expr,
    span::Span,
    ty,
    type_system::{Constraint, Type},
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
    Value(SharedValue),
    Function(fn(&Frame, &mut VecDeque<Frame>, &mut VecDeque<SharedValue>) -> eval::Result<()>),
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
    types: Vec<Option<Type>>,
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
        self.types.push(None);
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
        self.types[id.0] = Some(ty);
    }

    pub fn get_node(&self, id: NodeId) -> &Node {
        &self.nodes[id.0]
    }

    pub fn get_span(&self, id: NodeId) -> Span {
        self.spans[id.0]
    }

    pub fn get_type(&self, id: NodeId) -> &Option<Type> {
        &self.types[id.0]
    }

    pub fn add_operator(&mut self, symbol: &str, precedence: i32, associativity: Associativity) {
        self.operators.insert(
            symbol.into(),
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
        self.add_builtin(
            "||",
            ty!(Bool -> Bool -> Bool),
            BuiltinKind::Function(|frame, frames, _| {
                frames.push_back(frame.with_op(Op::Function(|frame, frames, results| {
                    let a = results.pop_back().unwrap();
                    let b = results.pop_back().unwrap();

                    match &*a.borrow() {
                        Value::Boolean(true) => {
                            frames.push_back(frame.with_op(Op::Value(a.clone())));
                        }
                        Value::Boolean(false) => {
                            frames.push_back(frame.with_op(Op::Value(b)));
                        }
                        _ => unreachable!(),
                    }

                    Ok(())
                })));
                frames.push_back(frame.with_op(Op::Force));

                Ok(())
            }),
        );

        self.add_builtin(
            "&&",
            ty!(Bool -> Bool -> Bool),
            BuiltinKind::Function(|frame, frames, _| {
                frames.push_back(frame.with_op(Op::Function(|frame, frames, results| {
                    let a = results.pop_back().unwrap();
                    let b = results.pop_back().unwrap();

                    match &*a.borrow() {
                        Value::Boolean(false) => {
                            frames.push_back(frame.with_op(Op::Value(a.clone())));
                        }
                        Value::Boolean(true) => {
                            frames.push_back(frame.with_op(Op::Value(b)));
                        }
                        _ => unreachable!(),
                    }

                    Ok(())
                })));
                frames.push_back(frame.with_op(Op::Force));

                Ok(())
            }),
        );

        self.add_builtin(
            "==",
            ty!(forall 0 . (Eq 0) => 0 -> 0 -> Bool),
            BuiltinKind::Function(|frame, frames, results| {
                let a = results.pop_back().unwrap();
                let b = results.pop_back().unwrap();

                match (&*a.borrow(), &*b.borrow()) {
                    (Value::Integer(a), Value::Integer(b)) => {
                        frames.push_back(
                            frame.with_op(Op::Value(SharedValue::new(Value::Boolean(a == b)))),
                        );
                    }
                    (Value::Boolean(a), Value::Boolean(b)) => {
                        frames.push_back(
                            frame.with_op(Op::Value(SharedValue::new(Value::Boolean(a == b)))),
                        );
                    }
                    (Value::Char(a), Value::Char(b)) => {
                        frames.push_back(
                            frame.with_op(Op::Value(SharedValue::new(Value::Boolean(a == b)))),
                        );
                    }
                    (Value::List(a), Value::List(b)) => match (a, b) {
                        (List::Nil, List::Nil) => {
                            frames.push_back(
                                frame.with_op(Op::Value(SharedValue::new(Value::Boolean(true)))),
                            );
                        }
                        (List::Nil, List::Cons(_, _)) | (List::Cons(_, _), List::Nil) => {
                            frames.push_back(
                                frame.with_op(Op::Value(SharedValue::new(Value::Boolean(false)))),
                            );
                        }
                        (List::Cons(h1, t1), List::Cons(h2, t2)) => {
                            frames.push_back(frame.with_op(Op::Function(
                                |frame, frames, results| {
                                    let value = results.pop_back().unwrap();

                                    match &*value.borrow() {
                                        Value::Boolean(true) => {
                                            frames.push_back(frame.with_op(Op::Infix("==".into())));
                                        }
                                        Value::Boolean(false) => {
                                            frames
                                                .push_back(frame.with_op(Op::Value(value.clone())));
                                        }
                                        _ => unreachable!(),
                                    }
                                    Ok(())
                                },
                            )));

                            frames.push_back(frame.with_op(Op::Infix("==".into())));
                            frames.push_back(frame.with_op(Op::Force));
                            frames.push_back(frame.with_op(Op::Value(h1.clone())));
                            frames.push_back(frame.with_op(Op::Force));
                            frames.push_back(frame.with_op(Op::Value(h2.clone())));
                            frames.push_back(frame.with_op(Op::Value(t1.clone())));
                            frames.push_back(frame.with_op(Op::Value(t2.clone())));
                        }
                    },
                    (Value::Thunk { .. }, _) | (_, Value::Thunk { .. }) => {
                        frames.push_back(frame.with_op(Op::Infix("==".into())));
                        frames.push_back(frame.with_op(Op::Force));
                        frames.push_back(frame.with_op(Op::Value(a.clone())));
                        frames.push_back(frame.with_op(Op::Force));
                        frames.push_back(frame.with_op(Op::Value(b.clone())));
                    }
                    _ => unreachable!(),
                }

                Ok(())
            }),
        );

        self.add_builtin(
            "!=",
            ty!(forall 0 . (Eq 0) => 0 -> 0 -> Bool),
            BuiltinKind::Function(|frame, frames, results| {
                let a = results.pop_back().unwrap();
                let b = results.pop_back().unwrap();

                match (&*a.borrow(), &*b.borrow()) {
                    (Value::Integer(a), Value::Integer(b)) => {
                        frames.push_back(
                            frame.with_op(Op::Value(SharedValue::new(Value::Boolean(a != b)))),
                        );
                    }
                    (Value::Boolean(a), Value::Boolean(b)) => {
                        frames.push_back(
                            frame.with_op(Op::Value(SharedValue::new(Value::Boolean(a != b)))),
                        );
                    }
                    (Value::Char(a), Value::Char(b)) => {
                        frames.push_back(
                            frame.with_op(Op::Value(SharedValue::new(Value::Boolean(a != b)))),
                        );
                    }
                    (Value::List(a), Value::List(b)) => match (a, b) {
                        (List::Nil, List::Nil) => {
                            frames.push_back(
                                frame.with_op(Op::Value(SharedValue::new(Value::Boolean(false)))),
                            );
                        }
                        (List::Nil, List::Cons(_, _)) | (List::Cons(_, _), List::Nil) => {
                            frames.push_back(
                                frame.with_op(Op::Value(SharedValue::new(Value::Boolean(true)))),
                            );
                        }
                        (List::Cons(h1, t1), List::Cons(h2, t2)) => {
                            frames.push_back(frame.with_op(Op::Function(
                                |frame, frames, results| {
                                    let value = results.pop_back().unwrap();

                                    match &*value.borrow() {
                                        Value::Boolean(true) => {
                                            frames
                                                .push_back(frame.with_op(Op::Value(value.clone())));
                                        }
                                        Value::Boolean(false) => {
                                            frames.push_back(frame.with_op(Op::Infix("!=".into())));
                                        }
                                        _ => unreachable!(),
                                    }
                                    Ok(())
                                },
                            )));

                            frames.push_back(frame.with_op(Op::Infix("!=".into())));
                            frames.push_back(frame.with_op(Op::Force));
                            frames.push_back(frame.with_op(Op::Value(h1.clone())));
                            frames.push_back(frame.with_op(Op::Force));
                            frames.push_back(frame.with_op(Op::Value(h2.clone())));
                            frames.push_back(frame.with_op(Op::Value(t1.clone())));
                            frames.push_back(frame.with_op(Op::Value(t2.clone())));
                        }
                    },
                    (Value::Thunk { .. }, _) | (_, Value::Thunk { .. }) => {
                        frames.push_back(frame.with_op(Op::Infix("!=".into())));
                        frames.push_back(frame.with_op(Op::Force));
                        frames.push_back(frame.with_op(Op::Value(a.clone())));
                        frames.push_back(frame.with_op(Op::Force));
                        frames.push_back(frame.with_op(Op::Value(b.clone())));
                    }
                    _ => unreachable!(),
                }

                Ok(())
            }),
        );

        self.add_builtin(
            "<",
            ty!(forall 0 . (Ord 0) => 0 -> 0 -> Bool),
            BuiltinKind::Function(|frame, frames, results| {
                let a = results.pop_back().unwrap();
                let b = results.pop_back().unwrap();

                match (&*a.borrow(), &*b.borrow()) {
                    (Value::Integer(a), Value::Integer(b)) => {
                        frames.push_back(
                            frame.with_op(Op::Value(SharedValue::new(Value::Boolean(a < b)))),
                        );
                    }
                    (Value::Boolean(a), Value::Boolean(b)) => {
                        frames.push_back(
                            frame.with_op(Op::Value(SharedValue::new(Value::Boolean(a < b)))),
                        );
                    }
                    (Value::Char(a), Value::Char(b)) => {
                        frames.push_back(
                            frame.with_op(Op::Value(SharedValue::new(Value::Boolean(a < b)))),
                        );
                    }
                    (Value::List(a), Value::List(b)) => match (a, b) {
                        (List::Nil, List::Nil) | (List::Cons(_, _), List::Nil) => {
                            frames.push_back(
                                frame.with_op(Op::Value(SharedValue::new(Value::Boolean(false)))),
                            );
                        }
                        (List::Nil, List::Cons(_, _)) => {
                            frames.push_back(
                                frame.with_op(Op::Value(SharedValue::new(Value::Boolean(true)))),
                            );
                        }
                        (List::Cons(h1, t1), List::Cons(h2, t2)) => {
                            frames.push_back(frame.with_op(Op::Infix("<".into())));
                            frames.push_back(frame.with_op(Op::Force));
                            frames.push_back(frame.with_op(Op::Value(h1.clone())));
                            frames.push_back(frame.with_op(Op::Force));
                            frames.push_back(frame.with_op(Op::Value(h2.clone())));
                            frames.push_back(frame.with_op(Op::Value(t1.clone())));
                            frames.push_back(frame.with_op(Op::Value(t2.clone())));
                        }
                    },
                    (Value::Thunk { .. }, _) | (_, Value::Thunk { .. }) => {
                        frames.push_back(frame.with_op(Op::Infix("<".into())));
                        frames.push_back(frame.with_op(Op::Force));
                        frames.push_back(frame.with_op(Op::Value(a.clone())));
                        frames.push_back(frame.with_op(Op::Force));
                        frames.push_back(frame.with_op(Op::Value(b.clone())));
                    }
                    _ => unreachable!(),
                }

                Ok(())
            }),
        );

        self.add_builtin(
            "<=",
            ty!(forall 0 . (Ord 0) => 0 -> 0 -> Bool),
            BuiltinKind::Function(|frame, frames, results| {
                let a = results.pop_back().unwrap();
                let b = results.pop_back().unwrap();

                match (&*a.borrow(), &*b.borrow()) {
                    (Value::Integer(a), Value::Integer(b)) => {
                        frames.push_back(
                            frame.with_op(Op::Value(SharedValue::new(Value::Boolean(a <= b)))),
                        );
                    }
                    (Value::Boolean(a), Value::Boolean(b)) => {
                        frames.push_back(
                            frame.with_op(Op::Value(SharedValue::new(Value::Boolean(a <= b)))),
                        );
                    }
                    (Value::Char(a), Value::Char(b)) => {
                        frames.push_back(
                            frame.with_op(Op::Value(SharedValue::new(Value::Boolean(a <= b)))),
                        );
                    }
                    (Value::List(a), Value::List(b)) => match (a, b) {
                        (List::Nil, List::Nil) | (List::Nil, List::Cons(_, _)) => {
                            frames.push_back(
                                frame.with_op(Op::Value(SharedValue::new(Value::Boolean(true)))),
                            );
                        }
                        (List::Cons(_, _), List::Nil) => {
                            frames.push_back(
                                frame.with_op(Op::Value(SharedValue::new(Value::Boolean(false)))),
                            );
                        }
                        (List::Cons(h1, t1), List::Cons(h2, t2)) => {
                            frames.push_back(frame.with_op(Op::Function(
                                |frame, frames, results| {
                                    let value = results.pop_back().unwrap();

                                    match &*value.borrow() {
                                        Value::Boolean(true) => {
                                            frames.push_back(frame.with_op(Op::Infix("<=".into())));
                                        }
                                        Value::Boolean(false) => {
                                            frames
                                                .push_back(frame.with_op(Op::Value(value.clone())));
                                        }
                                        _ => unreachable!(),
                                    }
                                    Ok(())
                                },
                            )));

                            frames.push_back(frame.with_op(Op::Infix("<=".into())));
                            frames.push_back(frame.with_op(Op::Force));
                            frames.push_back(frame.with_op(Op::Value(h1.clone())));
                            frames.push_back(frame.with_op(Op::Force));
                            frames.push_back(frame.with_op(Op::Value(h2.clone())));
                            frames.push_back(frame.with_op(Op::Value(t1.clone())));
                            frames.push_back(frame.with_op(Op::Value(t2.clone())));
                        }
                    },
                    (Value::Thunk { .. }, _) | (_, Value::Thunk { .. }) => {
                        frames.push_back(frame.with_op(Op::Infix("<=".into())));
                        frames.push_back(frame.with_op(Op::Force));
                        frames.push_back(frame.with_op(Op::Value(a.clone())));
                        frames.push_back(frame.with_op(Op::Force));
                        frames.push_back(frame.with_op(Op::Value(b.clone())));
                    }
                    _ => unreachable!(),
                }

                Ok(())
            }),
        );

        self.add_builtin(
            ">",
            ty!(forall 0 . (Ord 0) => 0 -> 0 -> Bool),
            BuiltinKind::Function(|frame, frames, results| {
                let a = results.pop_back().unwrap();
                let b = results.pop_back().unwrap();

                match (&*a.borrow(), &*b.borrow()) {
                    (Value::Integer(a), Value::Integer(b)) => {
                        frames.push_back(
                            frame.with_op(Op::Value(SharedValue::new(Value::Boolean(a > b)))),
                        );
                    }
                    (Value::Boolean(a), Value::Boolean(b)) => {
                        frames.push_back(
                            frame.with_op(Op::Value(SharedValue::new(Value::Boolean(a > b)))),
                        );
                    }
                    (Value::Char(a), Value::Char(b)) => {
                        frames.push_back(
                            frame.with_op(Op::Value(SharedValue::new(Value::Boolean(a > b)))),
                        );
                    }
                    (Value::List(a), Value::List(b)) => match (a, b) {
                        (List::Nil, List::Nil) | (List::Nil, List::Cons(_, _)) => {
                            frames.push_back(
                                frame.with_op(Op::Value(SharedValue::new(Value::Boolean(false)))),
                            );
                        }
                        (List::Cons(_, _), List::Nil) => {
                            frames.push_back(
                                frame.with_op(Op::Value(SharedValue::new(Value::Boolean(true)))),
                            );
                        }
                        (List::Cons(h1, t1), List::Cons(h2, t2)) => {
                            frames.push_back(frame.with_op(Op::Infix(">".into())));
                            frames.push_back(frame.with_op(Op::Force));
                            frames.push_back(frame.with_op(Op::Value(h1.clone())));
                            frames.push_back(frame.with_op(Op::Force));
                            frames.push_back(frame.with_op(Op::Value(h2.clone())));
                            frames.push_back(frame.with_op(Op::Value(t1.clone())));
                            frames.push_back(frame.with_op(Op::Value(t2.clone())));
                        }
                    },
                    (Value::Thunk { .. }, _) | (_, Value::Thunk { .. }) => {
                        frames.push_back(frame.with_op(Op::Infix(">".into())));
                        frames.push_back(frame.with_op(Op::Force));
                        frames.push_back(frame.with_op(Op::Value(a.clone())));
                        frames.push_back(frame.with_op(Op::Force));
                        frames.push_back(frame.with_op(Op::Value(b.clone())));
                    }
                    _ => unreachable!(),
                }

                Ok(())
            }),
        );

        self.add_builtin(
            ">=",
            ty!(forall 0 . (Ord 0) => 0 -> 0 -> Bool),
            BuiltinKind::Function(|frame, frames, results| {
                let a = results.pop_back().unwrap();
                let b = results.pop_back().unwrap();

                match (&*a.borrow(), &*b.borrow()) {
                    (Value::Integer(a), Value::Integer(b)) => {
                        frames.push_back(
                            frame.with_op(Op::Value(SharedValue::new(Value::Boolean(a >= b)))),
                        );
                    }
                    (Value::Boolean(a), Value::Boolean(b)) => {
                        frames.push_back(
                            frame.with_op(Op::Value(SharedValue::new(Value::Boolean(a >= b)))),
                        );
                    }
                    (Value::Char(a), Value::Char(b)) => {
                        frames.push_back(
                            frame.with_op(Op::Value(SharedValue::new(Value::Boolean(a >= b)))),
                        );
                    }
                    (Value::List(a), Value::List(b)) => match (a, b) {
                        (List::Nil, List::Nil) | (List::Cons(_, _), List::Nil) => {
                            frames.push_back(
                                frame.with_op(Op::Value(SharedValue::new(Value::Boolean(true)))),
                            );
                        }
                        (List::Nil, List::Cons(_, _)) => {
                            frames.push_back(
                                frame.with_op(Op::Value(SharedValue::new(Value::Boolean(false)))),
                            );
                        }
                        (List::Cons(h1, t1), List::Cons(h2, t2)) => {
                            frames.push_back(frame.with_op(Op::Function(
                                |frame, frames, results| {
                                    let value = results.pop_back().unwrap();

                                    match &*value.borrow() {
                                        Value::Boolean(true) => {
                                            frames.push_back(frame.with_op(Op::Infix(">=".into())));
                                        }
                                        Value::Boolean(false) => {
                                            frames
                                                .push_back(frame.with_op(Op::Value(value.clone())));
                                        }
                                        _ => unreachable!(),
                                    }
                                    Ok(())
                                },
                            )));

                            frames.push_back(frame.with_op(Op::Infix(">=".into())));
                            frames.push_back(frame.with_op(Op::Force));
                            frames.push_back(frame.with_op(Op::Value(h1.clone())));
                            frames.push_back(frame.with_op(Op::Force));
                            frames.push_back(frame.with_op(Op::Value(h2.clone())));
                            frames.push_back(frame.with_op(Op::Value(t1.clone())));
                            frames.push_back(frame.with_op(Op::Value(t2.clone())));
                        }
                    },
                    (Value::Thunk { .. }, _) | (_, Value::Thunk { .. }) => {
                        frames.push_back(frame.with_op(Op::Infix(">=".into())));
                        frames.push_back(frame.with_op(Op::Force));
                        frames.push_back(frame.with_op(Op::Value(a.clone())));
                        frames.push_back(frame.with_op(Op::Force));
                        frames.push_back(frame.with_op(Op::Value(b.clone())));
                    }
                    _ => unreachable!(),
                }

                Ok(())
            }),
        );

        self.add_builtin(
            "++",
            ty!(forall 0 . [0] -> [0] -> [0]),
            BuiltinKind::Function(|frame, frames, _| {
                frames.push_back(frame.with_op(Op::Function(|frame, frames, results| {
                    let a = results.pop_back().unwrap();
                    let b = results.pop_back().unwrap();

                    fn append(a: SharedValue, b: SharedValue) -> eval::Result<SharedValue> {
                        match &*a.borrow() {
                            Value::List(list) => match list {
                                List::Nil => Ok(b.clone()),
                                List::Cons(head, tail) => Ok(SharedValue::new(Value::List(
                                    List::Cons(head.clone(), append(tail.clone(), b)?),
                                ))),
                            },
                            _ => unreachable!(),
                        }
                    }
                    frames.push_back(frame.with_op(Op::Value(append(a, b)?)));
                    Ok(())
                })));
                frames.push_back(frame.with_op(Op::Force));

                Ok(())
            }),
        );

        self.add_builtin(
            ":",
            ty!(forall 0 . 0 -> [0] -> [0]),
            BuiltinKind::Function(|frame, frames, results| {
                let a = results.pop_back().unwrap();
                let b = results.pop_back().unwrap();

                frames.push_back(
                    frame.with_op(Op::Value(SharedValue::new(Value::List(List::Cons(a, b))))),
                );

                Ok(())
            }),
        );

        self.add_builtin(
            "+",
            ty!(forall 0 . (Num 0) => 0 -> 0 -> 0),
            BuiltinKind::Function(|frame, frames, results| {
                let a = results.pop_back().unwrap();
                let b = results.pop_back().unwrap();

                frames.push_back(frame.with_op(Op::Function(|frame, frames, results| {
                    let a = results.pop_back().unwrap();
                    let b = results.pop_back().unwrap();

                    match (&*a.borrow(), &*b.borrow()) {
                        (Value::Integer(a), Value::Integer(b)) => {
                            frames.push_back(
                                frame.with_op(Op::Value(SharedValue::new(Value::Integer(a + b)))),
                            );
                        }
                        _ => unreachable!(),
                    }
                    Ok(())
                })));
                frames.push_back(frame.with_op(Op::Force));
                frames.push_back(frame.with_op(Op::Value(a)));
                frames.push_back(frame.with_op(Op::Force));
                frames.push_back(frame.with_op(Op::Value(b)));

                Ok(())
            }),
        );

        self.add_builtin(
            "-",
            ty!(forall 0 . (Num 0) => 0 -> 0 -> 0),
            BuiltinKind::Function(|frame, frames, results| {
                let a = results.pop_back().unwrap();
                let b = results.pop_back().unwrap();

                frames.push_back(frame.with_op(Op::Function(|frame, frames, results| {
                    let a = results.pop_back().unwrap();
                    let b = results.pop_back().unwrap();

                    match (&*a.borrow(), &*b.borrow()) {
                        (Value::Integer(a), Value::Integer(b)) => {
                            frames.push_back(
                                frame.with_op(Op::Value(SharedValue::new(Value::Integer(a - b)))),
                            );
                        }
                        _ => unreachable!(),
                    }
                    Ok(())
                })));
                frames.push_back(frame.with_op(Op::Force));
                frames.push_back(frame.with_op(Op::Value(a)));
                frames.push_back(frame.with_op(Op::Force));
                frames.push_back(frame.with_op(Op::Value(b)));

                Ok(())
            }),
        );

        self.add_builtin(
            "*",
            ty!(forall 0 . (Num 0) => 0 -> 0 -> 0),
            BuiltinKind::Function(|frame, frames, results| {
                let a = results.pop_back().unwrap();
                let b = results.pop_back().unwrap();

                frames.push_back(frame.with_op(Op::Function(|frame, frames, results| {
                    let a = results.pop_back().unwrap();
                    let b = results.pop_back().unwrap();

                    match (&*a.borrow(), &*b.borrow()) {
                        (Value::Integer(a), Value::Integer(b)) => {
                            frames.push_back(
                                frame.with_op(Op::Value(SharedValue::new(Value::Integer(a * b)))),
                            );
                        }
                        _ => unreachable!(),
                    }
                    Ok(())
                })));
                frames.push_back(frame.with_op(Op::Force));
                frames.push_back(frame.with_op(Op::Value(a)));
                frames.push_back(frame.with_op(Op::Force));
                frames.push_back(frame.with_op(Op::Value(b)));

                Ok(())
            }),
        );

        self.add_builtin(
            "/",
            ty!(forall 0 . (Num 0) => 0 -> 0 -> 0),
            BuiltinKind::Function(|frame, frames, results| {
                let a = results.pop_back().unwrap();
                let b = results.pop_back().unwrap();

                frames.push_back(frame.with_op(Op::Function(|frame, frames, results| {
                    let a = results.pop_back().unwrap();
                    let b = results.pop_back().unwrap();

                    match (&*a.borrow(), &*b.borrow()) {
                        (Value::Integer(a), Value::Integer(b)) => {
                            frames.push_back(
                                frame.with_op(Op::Value(
                                    a.checked_div(*b)
                                        .map(|res| SharedValue::new(Value::Integer(res)))
                                        .ok_or(eval::Error::DivisionByZero(frame.span))?,
                                )),
                            );
                        }
                        _ => unreachable!(),
                    }
                    Ok(())
                })));
                frames.push_back(frame.with_op(Op::Force));
                frames.push_back(frame.with_op(Op::Value(a)));
                frames.push_back(frame.with_op(Op::Force));
                frames.push_back(frame.with_op(Op::Value(b)));

                Ok(())
            }),
        );

        self.add_builtin(
            "%",
            ty!(forall 0 . (Num 0) => 0 -> 0 -> 0),
            BuiltinKind::Function(|frame, frames, results| {
                let a = results.pop_back().unwrap();
                let b = results.pop_back().unwrap();

                frames.push_back(frame.with_op(Op::Function(|frame, frames, results| {
                    let a = results.pop_back().unwrap();
                    let b = results.pop_back().unwrap();

                    match (&*a.borrow(), &*b.borrow()) {
                        (Value::Integer(a), Value::Integer(b)) => {
                            frames.push_back(
                                frame.with_op(Op::Value(SharedValue::new(Value::Integer(a % b)))),
                            );
                        }
                        _ => unreachable!(),
                    }
                    Ok(())
                })));
                frames.push_back(frame.with_op(Op::Force));
                frames.push_back(frame.with_op(Op::Value(a)));
                frames.push_back(frame.with_op(Op::Force));
                frames.push_back(frame.with_op(Op::Value(b)));

                Ok(())
            }),
        );

        self.add_builtin(
            "args",
            ty!([[Char]]),
            BuiltinKind::Value({
                let mut args = List::Nil;
                for arg in std::env::args().skip(2).collect::<Vec<_>>().iter().rev() {
                    let mut string = List::Nil;
                    for ch in arg.chars().rev() {
                        string = string.push(SharedValue::new(Value::Char(ch)));
                    }
                    args = args.push(SharedValue::new(Value::List(string)));
                }
                SharedValue::new(Value::List(args))
            }),
        );

        self.add_builtin(
            "not",
            ty!(Bool -> Bool),
            BuiltinKind::Function(|frame, frames, _| {
                frames.push_back(frame.with_op(Op::Function(|frame, frames, results| {
                    let arg = results.pop_back().unwrap();

                    match &*arg.borrow() {
                        Value::Boolean(b) => {
                            frames.push_back(
                                frame.with_op(Op::Value(SharedValue::new(Value::Boolean(!b)))),
                            );
                        }
                        _ => unreachable!(),
                    }

                    Ok(())
                })));
                frames.push_back(frame.with_op(Op::Force));

                Ok(())
            }),
        );

        self.add_builtin(
            "read",
            ty!([Char] -> [Char]),
            BuiltinKind::Function(|frame, frames, _| {
                frames.push_back(frame.with_op(Op::Function(|frame, frames, results| {
                    let arg = results.pop_back().unwrap();

                    let path = {
                        let mut value = arg.clone();
                        let mut chars = Vec::new();
                        while let Value::List(list) = &*value.clone().borrow() {
                            let Some(head) = list.head() else { break };

                            if let Value::Char(ch) = &*head.borrow() {
                                chars.push(*ch)
                            }

                            value = match list.tail() {
                                Some(tail) => tail,
                                None => break,
                            };
                        }

                        chars.into_iter().collect::<String>()
                    };

                    let contents = if path == "-" {
                        use std::io::Read;
                        let mut input = String::new();
                        std::io::stdin().read_to_string(&mut input).map_err(|e| {
                            eval::Error::IO(format!("Failed to read from stdin: {}", e), frame.span)
                        })?;
                        input
                    } else {
                        std::fs::read_to_string(&path).map_err(|e| {
                            eval::Error::IO(
                                format!("Failed to read file '{}': {}", path, e),
                                frame.span,
                            )
                        })?
                    };

                    frames.push_back(
                        frame.with_op(Op::Value(SharedValue::new(Value::List(List::from(
                            contents
                                .chars()
                                .map(|ch| SharedValue::new(Value::Char(ch)))
                                .collect(),
                        ))))),
                    );

                    Ok(())
                })));

                frames.push_back(frame.with_op(Op::DisableFullForce));
                frames.push_back(frame.with_op(Op::Force));
                frames.push_back(frame.with_op(Op::EnableFullForce));

                Ok(())
            }),
        );

        self.add_builtin(
            "print",
            ty!(forall 0 . 0 -> 0),
            BuiltinKind::Function(|frame, frames, _| {
                frames.push_back(frame.with_op(Op::Function(|_, _, results| {
                    let arg = results.back().unwrap();
                    println!("{}", arg.borrow());
                    Ok(())
                })));
                frames.push_back(frame.with_op(Op::Force));
                Ok(())
            }),
        );

        self.add_builtin(
            "head",
            ty!(forall 0 . [0] -> 0),
            BuiltinKind::Function(|frame, frames, _| {
                frames.push_back(frame.with_op(Op::Function(|frame, _, results| {
                    let arg = results.pop_back().unwrap();
                    match &*arg.borrow() {
                        Value::List(list) => results
                            .push_back(list.head().ok_or(eval::Error::EmptyList(frame.span))?),
                        _ => unreachable!(),
                    }
                    Ok(())
                })));
                frames.push_back(frame.with_op(Op::Force));
                Ok(())
            }),
        );

        self.add_builtin(
            "tail",
            ty!(forall 0 . [0] -> [0]),
            BuiltinKind::Function(|frame, frames, _| {
                frames.push_back(frame.with_op(Op::Function(|frame, _, results| {
                    let arg = results.pop_back().unwrap();
                    match &*arg.borrow() {
                        Value::List(list) => results
                            .push_back(list.tail().ok_or(eval::Error::EmptyList(frame.span))?),
                        _ => unreachable!(),
                    }
                    Ok(())
                })));
                frames.push_back(frame.with_op(Op::Force));
                Ok(())
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
