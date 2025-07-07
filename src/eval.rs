use core::fmt;
use std::{collections::HashMap, result};

use crate::{
    ast::{Expr, InfixOp, PrefixOp, Stmt},
    span::{Span, Spanned},
    type_system::Typed,
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
        body: Spanned<Expr>,
        env: Env,
    },
    Thunk {
        expr: Spanned<Expr>,
    },
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Integer(n) => write!(f, "{n}"),
            Value::Boolean(b) => write!(f, "{b}"),
            Value::Function { param, body, .. } => {
                write!(f, "{} -> {}", param, body.value)
            }
            Value::Thunk { expr } => write!(f, "<thunk: {}>", expr.value),
        }
    }
}

pub type Env = HashMap<String, Value>;

fn force(value: Value, env: &mut Env) -> Result<Value> {
    match value {
        Value::Thunk { expr } => eval_expr(expr, env),
        v => Ok(v),
    }
}

fn eval_expr(expr: Spanned<Expr>, env: &mut Env) -> Result<Value> {
    match expr.value {
        Expr::Unit => Ok(Value::Unit),

        Expr::Boolean(bool) => Ok(Value::Boolean(bool)),

        Expr::Integer(n) => Ok(Value::Integer(n)),

        Expr::Identifier(name) => {
            let val = env.remove(&name).unwrap();
            let val = force(val, env)?;
            Ok(env.insert(name.clone(), val).unwrap())
        }

        Expr::Condition { cond, then, alt } => {
            let cond_val = eval_expr(*cond, env)?;
            match cond_val {
                Value::Boolean(true) => eval_expr(*then, env),
                Value::Boolean(false) => eval_expr(*alt, env),
                _ => unreachable!(),
            }
        }

        Expr::Infix { lhs, op, rhs } => {
            let lhs_val = eval_expr(*lhs, env)?;
            let rhs_val = eval_expr(*rhs, env)?;

            match (lhs_val, rhs_val) {
                (Value::Integer(a), Value::Integer(b)) => match op {
                    InfixOp::Add => Ok(Value::Integer(a + b)),
                    InfixOp::Sub => Ok(Value::Integer(a - b)),
                    InfixOp::Mul => Ok(Value::Integer(a * b)),
                    InfixOp::Div => a
                        .checked_div(b)
                        .map(Value::Integer)
                        .ok_or(Error::DivisionByZero(expr.span)),
                    InfixOp::Mod => Ok(Value::Integer(a % b)),
                    InfixOp::Eq => Ok(Value::Boolean(a == b)),
                },

                (Value::Boolean(a), Value::Boolean(b)) => match op {
                    InfixOp::Eq => Ok(Value::Boolean(a == b)),
                    _ => unreachable!(),
                },

                (Value::Unit, Value::Unit) => match op {
                    InfixOp::Eq => Ok(Value::Boolean(true)),
                    _ => unreachable!(),
                },

                _ => unreachable!(),
            }
        }

        Expr::Prefix { op, rhs } => {
            let rhs_val = eval_expr(*rhs, env)?;
            match (op, rhs_val) {
                (PrefixOp::Neg, Value::Integer(n)) => Ok(Value::Integer(-n)),
                _ => unreachable!(),
            }
        }

        Expr::Lambda { param, body } => Ok(Value::Function {
            param: param.value,
            body: *body,
            env: env.clone(),
        }),

        Expr::Application { func, arg } => {
            let func_val = eval_expr(*func, env)?;
            let arg_val = eval_expr(*arg, env)?;

            match func_val {
                Value::Function {
                    param,
                    body,
                    env: mut closure_env,
                } => {
                    closure_env.insert(param, arg_val);
                    eval_expr(body, &mut closure_env)
                }
                _ => unreachable!(),
            }
        }
    }
}

pub fn evaluate(ast: Vec<Typed<Spanned<Stmt>>>) -> Result<Value> {
    let mut env = Env::new();

    for node in ast {
        match node.value.value {
            Stmt::Let { name, expr } => {
                env.insert(name.clone(), Value::Thunk { expr });
            }

            Stmt::Expr(expr) => return eval_expr(expr, &mut env),
        }
    }

    Ok(Value::Unit)
}
