use core::fmt;
use std::{cell::RefCell, collections::VecDeque, ops::Deref, rc::Rc, result};

use fxhash::FxHashMap;
use itertools::Itertools;
use string_cache::DefaultAtom;

use crate::{
    context::{BuiltinKind, Context, Node, NodeId},
    expr::Expr,
    span::Span,
    type_system::Type,
};

#[derive(Debug)]
pub enum Error {
    DivisionByZero(Span),
    EmptyList(Span),
    IO(String, Span),
}

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug, Clone, Default)]
pub enum List {
    #[default]
    Nil,
    Cons(SharedValue, SharedValue),
}

impl List {
    pub fn from(values: Vec<SharedValue>) -> Self {
        let mut list = List::default();
        for value in values.into_iter().rev() {
            list = list.push(value);
        }
        list
    }

    pub fn head(&self) -> Option<SharedValue> {
        match self {
            List::Nil => None,
            List::Cons(head, _) => Some(head.clone()),
        }
    }

    pub fn tail(&self) -> Option<SharedValue> {
        match self {
            List::Nil => None,
            List::Cons(_, tail) => Some(tail.clone()),
        }
    }

    pub fn push(self, val: SharedValue) -> Self {
        List::Cons(val, SharedValue::new(Value::List(self)))
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Integer(i64),
    Char(char),
    Boolean(bool),
    List(List),
    Builtin(BuiltinKind),
    Function {
        param: String,
        body: NodeId,
        env: Env,
    },
    Thunk {
        expr: NodeId,
        env: Env,
        cached_value: Rc<RefCell<Option<Value>>>,
    },
}

impl Value {
    pub fn display<'a>(&'a self, context: &'a Context) -> Display<'a> {
        Display::new(self, context)
    }
}

#[derive(Debug, Clone)]
pub struct SharedValue(Rc<RefCell<Value>>);

impl SharedValue {
    pub fn new(value: Value) -> Self {
        Self(Rc::new(RefCell::new(value)))
    }
}

impl Deref for SharedValue {
    type Target = Rc<RefCell<Value>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct Display<'a> {
    value: &'a Value,
    context: &'a Context,
}

impl<'a> Display<'a> {
    pub fn new(value: &'a Value, context: &'a Context) -> Self {
        Self { value, context }
    }
}

impl<'a> fmt::Display for Display<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.value {
            Value::Unit => write!(f, "()"),
            Value::Integer(n) => write!(f, "{n}"),
            Value::Char(c) => write!(f, "'{}'", c.escape_default()),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::List(l) => match l {
                List::Nil => write!(f, "[]"),
                List::Cons(_, _) => {
                    let mut list = SharedValue::new(self.value.clone());
                    let mut values = Vec::new();

                    while let Value::List(l) = &*list.clone().borrow()
                        && let Some(h) = l.head()
                    {
                        values.push(h);
                        list = l
                            .tail()
                            .unwrap_or_else(|| SharedValue::new(Value::List(List::Nil)));
                    }

                    if values.is_empty() {
                        return write!(f, "[]");
                    }

                    match &*values[0].borrow() {
                        Value::Char(_) => {
                            let str = values
                                .iter()
                                .map(|v| {
                                    if let Value::Char(c) = &*v.borrow() {
                                        *c
                                    } else {
                                        unreachable!()
                                    }
                                })
                                .collect::<String>();
                            write!(f, "\"{}\"", str)
                        }
                        _ => {
                            let list = values
                                .iter()
                                .map(|v| v.borrow().display(self.context).to_string())
                                .join(", ");
                            write!(f, "[{}]", list)
                        }
                    }
                }
            },
            Value::Builtin(_) => {
                write!(f, "builtin")
            }
            Value::Function { param, body, .. } => {
                write!(f, "{} -> {}", param, body.display(self.context))
            }
            Value::Thunk { .. } => Ok(()),
        }
    }
}

pub type Env = Rc<RefCell<FxHashMap<DefaultAtom, SharedValue>>>;

#[derive(Debug, Clone, Default)]
pub struct State {
    pub full_force: bool,
}

#[derive(Clone)]
pub enum Op {
    Start(NodeId),
    Force,
    Infix(String),
    Cond,
    App,
    Nop,
    EnableFullForce,
    DisableFullForce,
    Value(SharedValue),
    Function(fn(&Context, &Frame, &mut VecDeque<Frame>, &mut VecDeque<SharedValue>) -> Result<()>),
}

#[derive(Clone)]
pub struct Frame {
    pub op: Op,
    pub env: Env,
    pub span: Span,
}

impl Frame {
    pub fn new(op: Op, env: Env, span: Span) -> Self {
        Self { op, env, span }
    }

    pub fn with_op(&self, op: Op) -> Self {
        Self {
            op,
            env: self.env.clone(),
            span: self.span,
        }
    }
}

pub fn eval_expr(ctx: &Context, frames: impl Into<VecDeque<Frame>>) -> Result<SharedValue> {
    let mut frames = frames.into() as VecDeque<Frame>;
    let mut results = VecDeque::<SharedValue>::new();
    let mut state = State::default();

    while let Some(frame) = frames.pop_back() {
        match frame.op {
            Op::Start(expr) => match ctx.get_node(expr).clone() {
                Node::Expr(Expr::Unit) => {
                    frames.push_back(frame.with_op(Op::Value(SharedValue::new(Value::Unit))));
                }
                Node::Expr(Expr::Boolean(bool)) => {
                    frames.push_back(
                        frame.with_op(Op::Value(SharedValue::new(Value::Boolean(bool)))),
                    );
                }
                Node::Expr(Expr::Integer(int)) => {
                    frames
                        .push_back(frame.with_op(Op::Value(SharedValue::new(Value::Integer(int)))));
                }
                Node::Expr(Expr::Char(char)) => {
                    frames.push_back(frame.with_op(Op::Value(SharedValue::new(Value::Char(char)))));
                }
                Node::Expr(Expr::Nil) => {
                    frames.push_back(
                        frame.with_op(Op::Value(SharedValue::new(Value::List(List::Nil)))),
                    );
                }
                Node::Expr(Expr::Cons { head, tail }) => {
                    let mut elems = Vec::new();
                    let mut cur_head = head;
                    let mut cur_tail = tail;

                    loop {
                        elems.push(SharedValue::new(Value::Thunk {
                            expr: cur_head,
                            env: frame.env.clone(),
                            cached_value: Rc::new(RefCell::new(None)),
                        }));
                        match ctx.get_node(cur_tail) {
                            Node::Expr(Expr::Cons { head, tail }) => {
                                cur_head = *head;
                                cur_tail = *tail;
                            }
                            _ => break,
                        }
                    }

                    frames.push_back(
                        frame.with_op(Op::Value(SharedValue::new(Value::List(List::from(elems))))),
                    );
                }
                Node::Expr(Expr::Lambda { param, body }) => {
                    let mut closure_env = FxHashMap::default();
                    for (k, v) in frame.env.borrow().iter() {
                        closure_env.insert(k.clone(), v.clone());
                    }
                    frames.push_back(frame.with_op(Op::Value(SharedValue::new(Value::Function {
                        param,
                        body,
                        env: Rc::new(RefCell::new(closure_env)),
                    }))));
                }
                Node::Expr(Expr::Identifier(id)) => {
                    frames.push_back(frame.with_op(Op::Force));
                    frames.push_back(frame.with_op(Op::Value(
                        frame.env.borrow().get(&id.into()).unwrap().clone(),
                    )));
                }
                Node::Expr(Expr::Infix { lhs, op, rhs }) => {
                    frames.push_back(frame.with_op(Op::Infix(op)));
                    frames.push_back(frame.with_op(Op::Value(SharedValue::new(Value::Thunk {
                        expr: lhs,
                        env: frame.env.clone(),
                        cached_value: Rc::new(RefCell::new(None)),
                    }))));
                    frames.push_back(frame.with_op(Op::Value(SharedValue::new(Value::Thunk {
                        expr: rhs,
                        env: frame.env.clone(),
                        cached_value: Rc::new(RefCell::new(None)),
                    }))));
                }
                Node::Expr(Expr::Condition { cond, then, alt }) => {
                    frames.push_back(frame.with_op(Op::Cond));
                    frames.push_back(frame.with_op(Op::Force));
                    frames.push_back(Frame::new(
                        Op::Start(cond),
                        frame.env.clone(),
                        ctx.get_span(cond),
                    ));
                    frames.push_back(frame.with_op(Op::Value(SharedValue::new(Value::Thunk {
                        expr: then,
                        env: frame.env.clone(),
                        cached_value: Rc::new(RefCell::new(None)),
                    }))));
                    frames.push_back(frame.with_op(Op::Value(SharedValue::new(Value::Thunk {
                        expr: alt,
                        env: frame.env.clone(),
                        cached_value: Rc::new(RefCell::new(None)),
                    }))));
                }
                Node::Expr(Expr::Application { func, arg }) => {
                    frames.push_back(frame.with_op(Op::App));
                    frames.push_back(frame.with_op(Op::Force));
                    frames.push_back(Frame::new(
                        Op::Start(func),
                        frame.env.clone(),
                        ctx.get_span(func),
                    ));
                    frames.push_back(frame.with_op(Op::Value(SharedValue::new(Value::Thunk {
                        expr: arg,
                        env: frame.env.clone(),
                        cached_value: Rc::new(RefCell::new(None)),
                    }))));
                }
                _ => unreachable!(),
            },
            Op::Force => {
                let value = results.pop_back().unwrap();

                match &*value.borrow() {
                    Value::Thunk {
                        expr,
                        env,
                        cached_value,
                    } => {
                        if let Some(cached) = &*cached_value.borrow() {
                            frames.push_back(
                                frame.with_op(Op::Value(SharedValue::new(cached.clone()))),
                            );
                        } else {
                            let frame =
                                Frame::new(Op::Start(*expr), env.clone(), ctx.get_span(*expr));
                            frames.push_back(frame.with_op(Op::Force));
                            frames.push_back(frame.with_op(Op::Function(
                                |_, frame, frames, results| {
                                    let thunk = results.pop_back().unwrap();
                                    let value = results.pop_back().unwrap();

                                    if let Value::Thunk { .. } = &*value.borrow() {
                                        frames.push_back(frame.with_op(Op::Function(
                                            |_, frame, frames, results| {
                                                let thunk = results.pop_back().unwrap();
                                                let value = results.pop_back().unwrap();

                                                if let Value::Thunk { cached_value, .. } =
                                                    &mut *thunk.borrow_mut()
                                                {
                                                    *cached_value.borrow_mut() =
                                                        Some(value.borrow().clone());
                                                }

                                                frames.push_back(
                                                    frame.with_op(Op::Value(value.clone())),
                                                );

                                                Ok(())
                                            },
                                        )));
                                        frames.push_back(frame.with_op(Op::Value(thunk.clone())));
                                        frames.push_back(frame.with_op(Op::Force));
                                        frames.push_back(frame.with_op(Op::Value(value.clone())));
                                    } else {
                                        if let Value::Thunk { cached_value, .. } =
                                            &mut *thunk.borrow_mut()
                                        {
                                            *cached_value.borrow_mut() =
                                                Some(value.borrow().clone());
                                        }
                                        frames.push_back(frame.with_op(Op::Value(value.clone())));
                                    }

                                    Ok(())
                                },
                            )));
                            frames.push_back(frame.with_op(Op::Value(value.clone())));
                            frames.push_back(frame);
                        }
                    }
                    Value::List(list) if state.full_force => match list {
                        List::Nil => frames.push_back(frame.with_op(Op::Value(value.clone()))),
                        List::Cons(head, tail) => {
                            frames.push_back(frame.with_op(Op::Function(
                                |_, frame, frames, results| {
                                    let head = results.pop_back().unwrap();
                                    let tail = results.pop_back().unwrap();

                                    frames.push_back(frame.with_op(Op::Value(SharedValue::new(
                                        Value::List(List::Cons(head.clone(), tail.clone())),
                                    ))));

                                    Ok(())
                                },
                            )));
                            frames.push_back(frame.with_op(Op::Force));
                            frames.push_back(frame.with_op(Op::Value(head.clone())));
                            frames.push_back(frame.with_op(Op::Force));
                            frames.push_back(frame.with_op(Op::Value(tail.clone())));
                        }
                    },
                    Value::Builtin(BuiltinKind::Value(value)) => {
                        frames.push_back(frame.with_op(Op::Value(value.clone())))
                    }
                    _ => frames.push_back(frame.with_op(Op::Value(value.clone()))),
                }
            }
            Op::Infix(ref op) => {
                match &*frame
                    .env
                    .borrow()
                    .get(&op.as_str().into())
                    .unwrap()
                    .borrow()
                {
                    Value::Builtin(BuiltinKind::Function(fun)) => {
                        fun(ctx, &frame, &mut frames, &mut results)?;
                    }
                    Value::Function {
                        param: a,
                        body,
                        env,
                    } => match ctx.get_node(*body) {
                        Node::Expr(Expr::Lambda { param: b, body }) => {
                            let lhs = results.pop_back().unwrap();
                            let rhs = results.pop_back().unwrap();

                            let mut new_env = env.borrow().clone();
                            new_env.insert(DefaultAtom::from(a.as_str()), lhs);
                            new_env.insert(DefaultAtom::from(b.as_str()), rhs);
                            frames.push_back(Frame {
                                op: Op::Start(*body),
                                env: Rc::new(RefCell::new(new_env)),
                                span: ctx.get_span(*body),
                            });
                        }
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
            }
            Op::Cond => {
                let cond = results.pop_back().unwrap();
                let then = results.pop_back().unwrap();
                let alt = results.pop_back().unwrap();
                frames.push_back(frame.with_op(Op::Value(match &*cond.borrow() {
                    Value::Boolean(true) => then,
                    Value::Boolean(false) => alt,
                    _ => unreachable!(),
                })));
            }
            Op::App => {
                let func = results.pop_back().unwrap();

                match &*func.borrow() {
                    Value::Builtin(BuiltinKind::Function(fun)) => {
                        fun(ctx, &frame, &mut frames, &mut results)?;
                    }
                    Value::Function { param, body, env } => {
                        let arg = results.pop_back().unwrap();
                        let mut new_env = env.borrow().clone();
                        new_env.insert(DefaultAtom::from(param.as_str()), arg);
                        frames.push_back(frame.with_op(Op::Value(SharedValue::new(
                            Value::Thunk {
                                expr: *body,
                                env: Rc::new(RefCell::new(new_env)),
                                cached_value: Rc::new(RefCell::new(None)),
                            },
                        ))));
                    }
                    _ => unreachable!(),
                }
            }
            Op::Nop => {}
            Op::Value(value) => {
                results.push_back(value);
            }
            Op::Function(fun) => {
                fun(ctx, &frame, &mut frames, &mut results)?;
            }
            Op::EnableFullForce => {
                state.full_force = true;
            }
            Op::DisableFullForce => {
                state.full_force = false;
            }
        }
    }

    Ok(results.pop_back().unwrap())
}

pub fn evaluate(ctx: &Context, nodes: &[NodeId]) -> Result<Value> {
    let env = Rc::new(RefCell::new(FxHashMap::default()));

    for (name, builtin) in ctx.builtins() {
        env.borrow_mut().insert(
            DefaultAtom::from(name.as_str()),
            SharedValue::new(Value::Builtin(builtin.kind.clone())),
        );
    }

    let mut value = SharedValue::new(Value::Unit);
    for node in nodes {
        match ctx.get_node(*node) {
            Node::Expr(_) => {
                let frame = Frame::new(Op::Start(*node), env.clone(), ctx.get_span(*node));
                value = eval_expr(
                    ctx,
                    [
                        frame.with_op(Op::Force),
                        frame.with_op(Op::EnableFullForce),
                        frame.with_op(Op::Force),
                        frame.with_op(match ctx.get_type(*node) {
                            Some(Type::List(_)) => Op::EnableFullForce,
                            _ => Op::Nop,
                        }),
                        frame,
                    ],
                )?;
            }
            Node::Bind(id, expr) => {
                let mut closure_env = FxHashMap::default();
                for (k, v) in env.borrow().iter() {
                    closure_env.insert(k.clone(), v.clone());
                }
                let closure_env = Rc::new(RefCell::new(closure_env));

                let val = match ctx.get_node(*expr) {
                    Node::Expr(Expr::Lambda { param, body }) => {
                        let func = Value::Function {
                            param: param.clone(),
                            body: *body,
                            env: closure_env.clone(),
                        };

                        closure_env.borrow_mut().insert(
                            DefaultAtom::from(id.as_str()),
                            SharedValue::new(func.clone()),
                        );

                        func
                    }
                    _ => Value::Thunk {
                        expr: *expr,
                        env: closure_env,
                        cached_value: Rc::new(RefCell::new(None)),
                    },
                };

                env.borrow_mut()
                    .insert(DefaultAtom::from(id.as_str()), SharedValue::new(val));
            }
            Node::Import(_) => continue,
        }
    }

    Ok(value.borrow().clone())
}
