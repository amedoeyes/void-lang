use core::fmt;
use std::{collections::HashMap, result};

use crate::{
    context::{Context, Node, NodeId},
    expr::{Expr, InfixOp, PrefixOp},
    span::Span,
};

#[derive(Debug)]
pub enum Error {
    DivisionByZero(Span),
}

type Result<T> = result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Integer(i64),
    Boolean(bool),
    Function {
        param: String,
        body: NodeId,
        env: Env,
    },
    Thunk {
        expr: NodeId,
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
            Value::Function { param, body, .. } => {
                write!(f, "{} -> {}", param, body.display(self.context))
            }
            Value::Thunk { expr } => write!(f, "<thunk: {}>", expr.display(self.context)),
        }
    }
}

pub type Env = HashMap<String, Value>;

fn force(ctx: &Context, env: &mut Env, value: Value) -> Result<Value> {
    match value {
        Value::Thunk { expr } => eval_expr(ctx, env, expr),
        v => Ok(v),
    }
}

fn eval_expr(ctx: &Context, env: &mut Env, expr: NodeId) -> Result<Value> {
    match ctx.get_node(expr) {
        Node::Expr(Expr::Unit) => Ok(Value::Unit),

        Node::Expr(Expr::Boolean(bool)) => Ok(Value::Boolean(*bool)),

        Node::Expr(Expr::Integer(n)) => Ok(Value::Integer(*n)),

        Node::Expr(Expr::Identifier(name)) => {
            let val = env.get(name).unwrap().clone();
            let val = force(ctx, env, val)?;
            env.insert(name.clone(), val.clone());
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
                Value::Function {
                    param,
                    body,
                    env: mut closure_env,
                } => {
                    closure_env.insert(param, arg_val);
                    eval_expr(ctx, &mut closure_env, body)
                }

                _ => unreachable!(),
            }
        }

        _ => unreachable!(),
    }
}

pub fn evaluate(ctx: &Context, nodes: &[NodeId]) -> Result<Value> {
    let mut env = Env::new();

    for node in nodes {
        match ctx.get_node(*node) {
            Node::Expr(_) => return eval_expr(ctx, &mut env, *node),
            Node::Bind(name, expr) => {
                env.insert(name.clone(), Value::Thunk { expr: *expr });
            }
        }
    }

    Ok(Value::Unit)
}
