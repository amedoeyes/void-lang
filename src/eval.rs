use core::fmt;
use std::{cell::RefCell, collections::VecDeque, rc::Rc, result};

use fxhash::FxHashMap;

use crate::{
    builtin::Builtins,
    context::{Context, Node, NodeId},
    expr::{Expr, InfixOp, PrefixOp},
    span::Span,
};

#[derive(Debug)]
pub enum Error {
    DivisionByZero(Span),
    EmptyList(Span),
}

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug, Clone, Default)]
pub enum List {
    #[default]
    Nil,
    Cons(Box<Value>, Box<List>),
}

impl List {
    pub fn from(values: Vec<Value>) -> Self {
        let mut list = List::default();
        for value in values.into_iter().rev() {
            list = list.push(value);
        }
        list
    }

    pub fn head(self) -> Option<Value> {
        match self {
            List::Nil => None,
            List::Cons(h, _) => Some(*h),
        }
    }

    pub fn tail(self) -> Option<List> {
        match self {
            List::Nil => None,
            List::Cons(_, t) => Some(*t),
        }
    }

    pub fn push(self, val: Value) -> Self {
        match self {
            List::Nil => Self::Cons(Box::new(val), Box::new(List::Nil)),
            List::Cons(_, _) => Self::Cons(Box::new(val), Box::new(self)),
        }
    }

    pub fn append(self, other: Self) -> Self {
        match self {
            List::Nil => other,
            List::Cons(h, t) => List::Cons(h, Box::new(t.append(other))),
        }
    }
}

impl PartialEq for List {
    fn eq(&self, other: &Self) -> bool {
        let mut a = self;
        let mut b = other;
        loop {
            match (a, b) {
                (Self::Nil, Self::Cons(_, _)) => return false,
                (Self::Cons(_, _), Self::Nil) => return false,
                (Self::Nil, Self::Nil) => return true,
                (Self::Cons(h1, t1), Self::Cons(h2, t2)) => {
                    match (h1.as_ref(), h2.as_ref()) {
                        (Value::Integer(x), Value::Integer(y)) => {
                            if x != y {
                                return false;
                            }
                        }
                        (Value::Boolean(x), Value::Boolean(y)) => {
                            if x != y {
                                return false;
                            }
                        }
                        _ => unreachable!(),
                    };
                    a = t1.as_ref();
                    b = t2.as_ref();
                }
            };
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Integer(i64),
    Boolean(bool),
    List(List),
    Builtin(fn(&Context, Value, Span) -> Result<Value>),
    Function {
        param: String,
        body: NodeId,
        env: Env,
    },
    Thunk {
        expr: NodeId,
        env: Env,
    },
}

impl Value {
    pub fn display<'a>(&self, context: &'a Context) -> Display<'a> {
        Display::new(self.clone(), context)
    }
}

pub struct Display<'a> {
    value: Value,
    context: &'a Context,
}

impl<'a> Display<'a> {
    pub fn new(value: Value, context: &'a Context) -> Self {
        Self { value, context }
    }
}

impl<'a> fmt::Display for Display<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.value {
            Value::Unit => write!(f, "()"),
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::List(l) => match l {
                List::Nil => write!(f, "[]"),
                List::Cons(h, t) => {
                    write!(f, "[{}", h.display(self.context))?;
                    let mut tail = t.as_ref();
                    while let List::Cons(h, t) = tail {
                        write!(f, ", {}", h.display(self.context))?;
                        tail = t.as_ref();
                    }
                    write!(f, "]")
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
                force(self.context, self.value.clone())
                    .unwrap()
                    .display(self.context)
            ),
        }
    }
}

pub type Env = Rc<RefCell<FxHashMap<String, Value>>>;

pub fn force(ctx: &Context, value: Value) -> Result<Value> {
    match value {
        Value::Thunk { expr, env } => eval_expr_stack(ctx, &env, expr),
        v => Ok(v),
    }
}

#[derive(Clone)]
enum EvalState {
    Start,
    Prefix(PrefixOp),
    Infix(InfixOp),
    Cond,
    Func,
    App(Value),
}

#[derive(Clone)]
struct EvalFrame {
    env: Env,
    expr: NodeId,
    state: EvalState,
}

pub fn eval_expr_stack(ctx: &Context, env: &Env, expr: NodeId) -> Result<Value> {
    let mut frame_stack = VecDeque::new();
    let mut result_stack = VecDeque::new();

    frame_stack.push_back(EvalFrame {
        env: env.clone(),
        expr,
        state: EvalState::Start,
    });

    while let Some(frame) = frame_stack.pop_back() {
        match frame.state {
            EvalState::Start => match ctx.get_node(frame.expr).clone() {
                Node::Expr(Expr::Unit) => {
                    result_stack.push_back(Value::Unit);
                }

                Node::Expr(Expr::Boolean(bool)) => {
                    result_stack.push_back(Value::Boolean(bool));
                }

                Node::Expr(Expr::Integer(n)) => {
                    result_stack.push_back(Value::Integer(n));
                }

                Node::Expr(Expr::Nil) => {
                    result_stack.push_back(Value::List(List::Nil));
                }

                Node::Expr(Expr::Cons { head, tail }) => {
                    let mut elems = Vec::new();
                    let mut cur_head = head;
                    let mut cur_tail = tail;

                    loop {
                        elems.push(Value::Thunk {
                            expr: cur_head,
                            env: frame.env.clone(),
                        });
                        match ctx.get_node(cur_tail) {
                            Node::Expr(Expr::Cons { head, tail }) => {
                                cur_head = *head;
                                cur_tail = *tail;
                            }
                            _ => break,
                        }
                    }

                    result_stack.push_back(Value::List(List::from(elems)));
                }

                Node::Expr(Expr::Lambda { param, body }) => match ctx.get_node(param) {
                    Node::Expr(Expr::Identifier(id)) => result_stack.push_back(Value::Function {
                        param: id.clone(),
                        body,
                        env: frame.env.clone(),
                    }),
                    _ => unreachable!(),
                },

                Node::Expr(Expr::Identifier(name)) => {
                    let val = frame.env.borrow().get(&name).cloned().unwrap();
                    let val = force(ctx, val)?;
                    frame.env.borrow_mut().insert(name.clone(), val.clone());
                    result_stack.push_back(val);
                }

                Node::Expr(Expr::Prefix { op, rhs }) => {
                    frame_stack.push_back(EvalFrame {
                        state: EvalState::Prefix(op),
                        ..frame.clone()
                    });

                    frame_stack.push_back(EvalFrame {
                        expr: rhs,
                        state: EvalState::Start,
                        ..frame.clone()
                    });
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

            EvalState::Prefix(op) => {
                let rhs = result_stack.pop_back().unwrap();

                let res = match (op, rhs) {
                    (PrefixOp::Neg, Value::Integer(n)) => Value::Integer(-n),
                    (PrefixOp::Not, Value::Boolean(b)) => Value::Boolean(!b),
                    _ => unreachable!(),
                };

                result_stack.push_back(res);
            }

            EvalState::Infix(op) => {
                let lhs = result_stack.pop_back().unwrap();
                let rhs = result_stack.pop_back().unwrap();

                let res = match (lhs, rhs) {
                    (Value::Integer(a), Value::Integer(b)) => match op {
                        InfixOp::Add => Value::Integer(a + b),
                        InfixOp::Sub => Value::Integer(a - b),
                        InfixOp::Mul => Value::Integer(a * b),
                        InfixOp::Div => a
                            .checked_div(b)
                            .map(Value::Integer)
                            .ok_or(Error::DivisionByZero(*ctx.get_span(expr)))?,
                        InfixOp::Mod => Value::Integer(a % b),
                        InfixOp::Eq => Value::Boolean(a == b),
                        InfixOp::Neq => Value::Boolean(a != b),

                        InfixOp::Lt => Value::Boolean(a < b),
                        InfixOp::Lte => Value::Boolean(a <= b),
                        InfixOp::Gt => Value::Boolean(a > b),
                        InfixOp::Gte => Value::Boolean(a >= b),

                        _ => unreachable!(),
                    },

                    (Value::Boolean(a), Value::Boolean(b)) => match op {
                        InfixOp::Eq => Value::Boolean(a == b),
                        InfixOp::Neq => Value::Boolean(a != b),
                        InfixOp::And => Value::Boolean(a && b),
                        InfixOp::Or => Value::Boolean(a || b),
                        _ => unreachable!(),
                    },

                    (Value::List(a), Value::List(b)) => match op {
                        InfixOp::Eq => Value::Boolean(a == b),
                        InfixOp::Neq => Value::Boolean(a != b),
                        InfixOp::Cons => Value::List(b.push(Value::List(a))),
                        InfixOp::Append => Value::List(b.append(a)),
                        _ => unreachable!(),
                    },

                    (a, Value::List(b)) => match op {
                        InfixOp::Cons => Value::List(b.push(a)),
                        _ => unreachable!(),
                    },

                    (Value::Unit, Value::Unit) => match op {
                        InfixOp::Eq => Value::Boolean(true),
                        _ => unreachable!(),
                    },

                    _ => unreachable!(),
                };

                result_stack.push_back(res);
            }

            EvalState::Cond => {
                let cond = result_stack.pop_back().unwrap();

                if let Node::Expr(Expr::Condition { then, alt, .. }) = ctx.get_node(frame.expr) {
                    match cond {
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
                let func = result_stack.pop_back().unwrap();

                if let Node::Expr(Expr::Application { arg, .. }) = ctx.get_node(frame.expr) {
                    frame_stack.push_back(EvalFrame {
                        state: EvalState::App(func),
                        ..frame.clone()
                    });

                    frame_stack.push_back(EvalFrame {
                        expr: *arg,
                        state: EvalState::Start,
                        ..frame
                    });
                }
            }
            EvalState::App(func) => {
                let arg = result_stack.pop_back().unwrap();

                match func {
                    Value::Builtin(f) => {
                        let result = f(ctx, arg, *ctx.get_span(frame.expr))?;
                        result_stack.push_back(result);
                    }
                    Value::Function { param, body, env } => {
                        let mut closure_env = FxHashMap::default();
                        for (k, v) in env.borrow().iter() {
                            closure_env.insert(k.clone(), v.clone());
                        }
                        closure_env.insert(param, arg);

                        frame_stack.push_back(EvalFrame {
                            env: Rc::new(RefCell::new(closure_env)),
                            expr: body,
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

pub fn evaluate(ctx: &Context, builtins: &Builtins, nodes: &[NodeId]) -> Result<Value> {
    let env = Rc::new(RefCell::new(FxHashMap::default()));

    for (name, builtin) in builtins {
        env.borrow_mut()
            .insert(name.clone(), Value::Builtin(builtin.eval));
    }

    for node in nodes {
        match ctx.get_node(*node) {
            Node::Expr(_) => return eval_expr_stack(ctx, &env, *node),
            Node::Bind(name, expr) => {
                env.borrow_mut().insert(
                    name.clone(),
                    Value::Thunk {
                        expr: *expr,
                        env: env.clone(),
                    },
                );
            }
        }
    }

    Ok(Value::Unit)
}
