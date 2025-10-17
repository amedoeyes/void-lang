use core::fmt;
use std::{cell::RefCell, collections::VecDeque, ops::Deref, rc::Rc, result};

use fxhash::FxHashMap;
use itertools::Itertools;
use string_cache::DefaultAtom;

use crate::{
    context::{BuiltinKind, Context, Node, NodeId},
    expr::Expr,
    span::Span,
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
    Cons(Rc<Value>, Rc<RefCell<List>>),
}

impl List {
    pub fn from(values: Vec<Value>) -> Self {
        let mut list = List::default();
        for value in values.into_iter().rev() {
            list = list.push(value);
        }
        list
    }

    pub fn head(&self) -> Option<Value> {
        match self {
            List::Nil => None,
            List::Cons(h, _) => Some(h.deref().clone()),
        }
    }

    pub fn tail(&self) -> Option<List> {
        match self {
            List::Nil => None,
            List::Cons(_, t) => Some(t.borrow().clone()),
        }
    }

    pub fn push(self, val: Value) -> Self {
        List::Cons(Rc::new(val), Rc::new(RefCell::new(self)))
    }

    pub fn append(self, other: List) -> Self {
        match self {
            List::Nil => other,
            List::Cons(h, t) => {
                List::Cons(h, Rc::new(RefCell::new(t.borrow().clone().append(other))))
            }
        }
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

impl Value {
    pub fn force(&mut self, ctx: &Context) -> Result<Value> {
        match self {
            Value::Thunk {
                expr,
                env,
                cached_value,
            } => {
                let mut cache = cached_value.borrow_mut();
                match cache.deref() {
                    Some(v) => Ok(v.clone()),
                    None => {
                        let val = eval_expr_stack(
                            ctx,
                            EvalFrame {
                                env: env.clone(),
                                expr: *expr,
                                state: EvalState::Start,
                            },
                        )?;
                        *cache = Some(val.borrow().clone());
                        Ok(val.borrow().clone())
                    }
                }
            }
            Value::Builtin(BuiltinKind::Value(v)) => Ok(*v.clone()),
            v => Ok(v.clone()),
        }
    }

    pub fn display<'a>(&'a self, context: &'a Context) -> Display<'a> {
        Display::new(self, context)
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
                    let mut list = l.clone();
                    let mut values = Vec::new();

                    while let Some(mut h) = list.head() {
                        values.push(h.force(self.context).unwrap());
                        list = list.tail().unwrap_or_default();
                    }

                    if values.is_empty() {
                        return write!(f, "[]");
                    }

                    match &values[0] {
                        Value::Char(_) => {
                            let str = values
                                .into_iter()
                                .map(|v| {
                                    if let Value::Char(c) = v {
                                        c
                                    } else {
                                        unreachable!()
                                    }
                                })
                                .collect::<String>();
                            write!(f, "\"{}\"", str)
                        }
                        _ => {
                            let list = values
                                .into_iter()
                                .map(|v| v.display(self.context).to_string())
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

            Value::Thunk { .. } => write!(
                f,
                "{}",
                self.value
                    .clone()
                    .force(self.context)
                    .unwrap()
                    .display(self.context)
            ),
        }
    }
}

pub type Env = Rc<RefCell<FxHashMap<DefaultAtom, SharedValue>>>;

#[derive(Clone)]
enum EvalState {
    Start,
    Infix(String),
    Cond,
    Func,
    App,
}

#[derive(Clone)]
struct EvalFrame {
    env: Env,
    expr: NodeId,
    state: EvalState,
}

fn eval_expr_stack(ctx: &Context, frame: EvalFrame) -> Result<SharedValue> {
    let mut frame_stack = VecDeque::from([frame]);
    let mut result_stack = VecDeque::new();

    while let Some(frame) = frame_stack.pop_back() {
        match frame.state {
            EvalState::Start => match ctx.get_node(frame.expr).clone() {
                Node::Expr(Expr::Unit) => {
                    result_stack.push_back(SharedValue::new(Value::Unit));
                }

                Node::Expr(Expr::Boolean(bool)) => {
                    result_stack.push_back(SharedValue::new(Value::Boolean(bool)));
                }

                Node::Expr(Expr::Integer(n)) => {
                    result_stack.push_back(SharedValue::new(Value::Integer(n)));
                }

                Node::Expr(Expr::Char(c)) => {
                    result_stack.push_back(SharedValue::new(Value::Char(c)));
                }

                Node::Expr(Expr::Nil) => {
                    result_stack.push_back(SharedValue::new(Value::List(List::Nil)));
                }

                Node::Expr(Expr::Cons { head, tail }) => {
                    let mut elems = Vec::new();
                    let mut cur_head = head;
                    let mut cur_tail = tail;

                    loop {
                        elems.push(Value::Thunk {
                            expr: cur_head,
                            env: frame.env.clone(),
                            cached_value: Rc::new(RefCell::new(None)),
                        });
                        match ctx.get_node(cur_tail) {
                            Node::Expr(Expr::Cons { head, tail }) => {
                                cur_head = *head;
                                cur_tail = *tail;
                            }
                            _ => break,
                        }
                    }

                    result_stack.push_back(SharedValue::new(Value::List(List::from(elems))));
                }

                Node::Expr(Expr::Lambda { param, body }) => {
                    let mut closure_env = FxHashMap::default();
                    for (k, v) in frame.env.borrow().iter() {
                        closure_env.insert(k.clone(), v.clone());
                    }
                    result_stack.push_back(SharedValue::new(Value::Function {
                        param,
                        body,
                        env: Rc::new(RefCell::new(closure_env)),
                    }))
                }

                Node::Expr(Expr::Identifier(name)) => {
                    result_stack.push_back(SharedValue::new(
                        frame
                            .env
                            .borrow()
                            .get(&name.into())
                            .unwrap()
                            .borrow_mut()
                            .force(ctx)?,
                    ));
                }

                Node::Expr(Expr::Infix { lhs, op, rhs }) => {
                    frame_stack.push_back(EvalFrame {
                        state: EvalState::Infix(op),
                        ..frame.clone()
                    });

                    frame_stack.push_back(EvalFrame {
                        expr: lhs,
                        state: EvalState::Start,
                        ..frame.clone()
                    });

                    frame_stack.push_back(EvalFrame {
                        expr: rhs,
                        state: EvalState::Start,
                        ..frame
                    });
                }

                Node::Expr(Expr::Condition { cond, .. }) => {
                    frame_stack.push_back(EvalFrame {
                        state: EvalState::Cond,
                        ..frame.clone()
                    });

                    frame_stack.push_back(EvalFrame {
                        expr: cond,
                        state: EvalState::Start,
                        ..frame
                    });
                }

                Node::Expr(Expr::Application { func, .. }) => {
                    frame_stack.push_back(EvalFrame {
                        state: EvalState::Func,
                        ..frame.clone()
                    });

                    frame_stack.push_back(EvalFrame {
                        expr: func,
                        state: EvalState::Start,
                        ..frame
                    });
                }

                _ => unreachable!(),
            },

            EvalState::Infix(op) => {
                let lhs = result_stack.pop_back().unwrap();
                let rhs = result_stack.pop_back().unwrap();

                match frame
                    .env
                    .borrow()
                    .get(&op.into())
                    .unwrap()
                    .borrow_mut()
                    .force(ctx)?
                {
                    Value::Builtin(BuiltinKind::Operator(f)) => {
                        let res = f(
                            ctx,
                            (&lhs.borrow_mut().force(ctx)?, ctx.get_span(frame.expr)),
                            (&rhs.borrow_mut().force(ctx)?, ctx.get_span(frame.expr)),
                        )?;
                        result_stack.push_back(SharedValue::new(res));
                    }
                    Value::Function {
                        param: a,
                        body,
                        env,
                    } => match ctx.get_node(body) {
                        Node::Expr(Expr::Lambda { param: b, body }) => {
                            let mut new_env = env.borrow().clone();
                            new_env.insert(DefaultAtom::from(a.as_str()), lhs);
                            new_env.insert(DefaultAtom::from(b.as_str()), rhs);
                            frame_stack.push_back(EvalFrame {
                                env: Rc::new(RefCell::new(new_env)),
                                expr: *body,
                                state: EvalState::Start,
                            });
                        }
                        _ => unreachable!(),
                    },

                    _ => unreachable!(),
                };
            }

            EvalState::Cond => {
                let cond = result_stack.pop_back().unwrap();
                let cond = cond.borrow();

                if let Node::Expr(Expr::Condition { then, alt, .. }) = ctx.get_node(frame.expr) {
                    match &*cond {
                        Value::Boolean(true) => {
                            frame_stack.push_back(EvalFrame {
                                expr: *then,
                                state: EvalState::Start,
                                ..frame
                            });
                        }
                        Value::Boolean(false) => {
                            frame_stack.push_back(EvalFrame {
                                expr: *alt,
                                state: EvalState::Start,
                                ..frame
                            });
                        }
                        _ => unreachable!(),
                    }
                }
            }

            EvalState::Func => {
                if let Node::Expr(Expr::Application { arg, .. }) = ctx.get_node(frame.expr) {
                    frame_stack.push_back(EvalFrame {
                        state: EvalState::App,
                        ..frame.clone()
                    });

                    frame_stack.push_back(EvalFrame {
                        expr: *arg,
                        state: EvalState::Start,
                        ..frame
                    });
                }
            }

            EvalState::App => {
                let arg = result_stack.pop_back().unwrap();
                let func = result_stack.pop_back().unwrap();
                let func = func.borrow();

                match &*func {
                    Value::Builtin(BuiltinKind::Function(f)) => {
                        result_stack.push_back(
                            f(ctx, &arg.borrow(), ctx.get_span(frame.expr))
                                .map(SharedValue::new)?,
                        );
                    }

                    Value::Function { param, body, env } => {
                        env.borrow_mut()
                            .insert(DefaultAtom::from(param.as_str()), arg);
                        frame_stack.push_back(EvalFrame {
                            env: Rc::new(RefCell::new(env.borrow().clone())),
                            expr: *body,
                            state: EvalState::Start,
                        });
                    }

                    _ => unreachable!(),
                }
            }
        }
    }

    Ok(result_stack.pop_back().unwrap())
}

pub fn evaluate(ctx: &Context, nodes: &[NodeId]) -> Result<Value> {
    let env = Rc::new(RefCell::new(FxHashMap::default()));

    for (name, builtin) in ctx.builtins() {
        env.borrow_mut().insert(
            DefaultAtom::from(name.as_str()),
            SharedValue::new(Value::Builtin(builtin.kind.clone())),
        );
    }

    let mut val = Value::Unit;
    for node in nodes {
        match ctx.get_node(*node) {
            Node::Expr(_) => {
                val = eval_expr_stack(
                    ctx,
                    EvalFrame {
                        env: env.clone(),
                        expr: *node,
                        state: EvalState::Start,
                    },
                )?
                .borrow()
                .clone();
            }
            Node::Bind(name, expr) => {
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
                            DefaultAtom::from(name.as_str()),
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
                    .insert(DefaultAtom::from(name.as_str()), SharedValue::new(val));
            }
        }
    }

    Ok(val)
}
