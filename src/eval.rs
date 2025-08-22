use core::fmt;
use std::{cell::RefCell, collections::HashMap, rc::Rc, result};

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

pub type Env = Rc<RefCell<HashMap<String, Value>>>;

pub fn force(ctx: &Context, value: Value) -> Result<Value> {
    match value {
        Value::Thunk { expr, env } => eval_expr(ctx, &env, expr),
        v => Ok(v),
    }
}

pub fn eval_expr(ctx: &Context, env: &Env, expr: NodeId) -> Result<Value> {
    match ctx.get_node(expr) {
        Node::Expr(Expr::Unit) => Ok(Value::Unit),

        Node::Expr(Expr::Boolean(bool)) => Ok(Value::Boolean(*bool)),

        Node::Expr(Expr::Integer(n)) => Ok(Value::Integer(*n)),

        Node::Expr(Expr::Nil) => Ok(Value::List(List::Nil)),

        Node::Expr(Expr::Cons { head, tail }) => {
            let mut elems = Vec::new();
            let mut cur_head = *head;
            let mut cur_tail = *tail;

            loop {
                elems.push(Value::Thunk {
                    expr: cur_head,
                    env: env.clone(),
                });
                match ctx.get_node(cur_tail) {
                    Node::Expr(Expr::Cons { head, tail }) => {
                        cur_head = *head;
                        cur_tail = *tail;
                    }
                    _ => break,
                }
            }

            Ok(Value::List(List::from(elems)))
        }

        Node::Expr(Expr::Identifier(name)) => {
            let val = env.borrow().get(name).cloned().unwrap();
            let val = force(ctx, val)?;
            env.borrow_mut().insert(name.clone(), val.clone());
            Ok(val)
        }

        Node::Expr(Expr::Condition { cond, then, alt }) => {
            let cond_val = eval_expr(ctx, env, *cond)?;
            match cond_val {
                Value::Boolean(true) => eval_expr(ctx, env, *then),
                Value::Boolean(false) => eval_expr(ctx, env, *alt),
                _ => unreachable!(),
            }
        }

        Node::Expr(Expr::Infix { lhs, op, rhs }) => {
            let lhs_val = eval_expr(ctx, env, *lhs)?;
            let rhs_val = eval_expr(ctx, env, *rhs)?;

            match (lhs_val, rhs_val) {
                (Value::Integer(a), Value::Integer(b)) => match op {
                    InfixOp::Add => Ok(Value::Integer(a + b)),
                    InfixOp::Sub => Ok(Value::Integer(a - b)),
                    InfixOp::Mul => Ok(Value::Integer(a * b)),
                    InfixOp::Div => a
                        .checked_div(b)
                        .map(Value::Integer)
                        .ok_or(Error::DivisionByZero(*ctx.get_span(expr))),
                    InfixOp::Mod => Ok(Value::Integer(a % b)),
                    InfixOp::Eq => Ok(Value::Boolean(a == b)),
                    InfixOp::Neq => Ok(Value::Boolean(a != b)),

                    InfixOp::Lt => Ok(Value::Boolean(a < b)),
                    InfixOp::Lte => Ok(Value::Boolean(a <= b)),
                    InfixOp::Gt => Ok(Value::Boolean(a > b)),
                    InfixOp::Gte => Ok(Value::Boolean(a >= b)),

                    _ => unreachable!(),
                },

                (Value::Boolean(a), Value::Boolean(b)) => match op {
                    InfixOp::Eq => Ok(Value::Boolean(a == b)),
                    InfixOp::Neq => Ok(Value::Boolean(a != b)),
                    InfixOp::And => Ok(Value::Boolean(a && b)),
                    InfixOp::Or => Ok(Value::Boolean(a || b)),
                    _ => unreachable!(),
                },

                (Value::List(a), Value::List(b)) => match op {
                    InfixOp::Eq => Ok(Value::Boolean(a == b)),
                    InfixOp::Neq => Ok(Value::Boolean(a != b)),
                    InfixOp::Cons => Ok(Value::List(b.push(Value::List(a)))),
                    InfixOp::Append => Ok(Value::List(a.append(b))),
                    _ => unreachable!(),
                },

                (a, Value::List(b)) => match op {
                    InfixOp::Cons => Ok(Value::List(b.push(a))),
                    _ => unreachable!(),
                },

                (Value::Unit, Value::Unit) => match op {
                    InfixOp::Eq => Ok(Value::Boolean(true)),
                    _ => unreachable!(),
                },

                _ => unreachable!(),
            }
        }

        Node::Expr(Expr::Prefix { op, rhs }) => {
            let rhs_val = eval_expr(ctx, env, *rhs)?;
            match (op, rhs_val) {
                (PrefixOp::Neg, Value::Integer(n)) => Ok(Value::Integer(-n)),
                (PrefixOp::Not, Value::Boolean(b)) => Ok(Value::Boolean(!b)),
                _ => unreachable!(),
            }
        }

        Node::Expr(Expr::Lambda { param, body }) => match ctx.get_node(*param) {
            Node::Expr(Expr::Identifier(id)) => Ok(Value::Function {
                param: id.clone(),
                body: *body,
                env: env.clone(),
            }),

            _ => unreachable!(),
        },

        Node::Expr(Expr::Application { func, arg }) => {
            let func_val = eval_expr(ctx, env, *func)?;
            let arg_val = eval_expr(ctx, env, *arg)?;

            match func_val {
                Value::Builtin(f) => f(ctx, arg_val, *ctx.get_span(expr)),
                Value::Function { param, body, env } => {
                    let mut closure_env = HashMap::new();
                    for (k, v) in env.borrow().iter() {
                        closure_env.insert(k.clone(), v.clone());
                    }
                    closure_env.insert(param, arg_val);
                    eval_expr(ctx, &Rc::new(RefCell::new(closure_env)), body)
                }

                _ => unreachable!(),
            }
        }

        _ => unreachable!(),
    }
}

pub fn evaluate(ctx: &Context, builtins: &Builtins, nodes: &[NodeId]) -> Result<Value> {
    let env = Rc::new(RefCell::new(HashMap::new()));

    for (name, builtin) in builtins {
        env.borrow_mut()
            .insert(name.clone(), Value::Builtin(builtin.eval));
    }

    for node in nodes {
        match ctx.get_node(*node) {
            Node::Expr(_) => return eval_expr(ctx, &env, *node),
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
