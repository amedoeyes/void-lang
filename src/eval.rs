use core::fmt;
use std::{cell::RefCell, cmp::Ordering, collections::VecDeque, ops::Deref, rc::Rc, result};

use fxhash::FxHashMap;
use itertools::Itertools;
use string_cache::DefaultAtom;

use crate::{
    context::{BuiltinKind, Context, Node, NodeId},
    expr::{Expr, InfixOp, PrefixOp},
    span::Span,
};

#[derive(Debug)]
pub enum Error {
    DivisionByZero(Span),
    EmptyList(Span),
    IOError(String, Span),
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

pub fn add_values(ctx: &Context, v1: &mut Value, v2: &mut Value) -> Result<Value> {
    let v1 = v1.force(ctx)?;
    let v2 = v2.force(ctx)?;

    match (v1, v2) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
        _ => unreachable!(),
    }
}

pub fn sub_values(ctx: &Context, v1: &mut Value, v2: &mut Value) -> Result<Value> {
    let v1 = v1.force(ctx)?;
    let v2 = v2.force(ctx)?;

    match (v1, v2) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a - b)),
        _ => unreachable!(),
    }
}

pub fn div_values(ctx: &Context, v1: &mut Value, v2: &mut Value) -> Result<Value> {
    let v1 = v1.force(ctx)?;
    let v2 = v2.force(ctx)?;

    match (v1, v2) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a / b)),
        _ => unreachable!(),
    }
}

pub fn mul_values(ctx: &Context, v1: &mut Value, v2: &mut Value) -> Result<Value> {
    let v1 = v1.force(ctx)?;
    let v2 = v2.force(ctx)?;

    match (v1, v2) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a * b)),
        _ => unreachable!(),
    }
}

pub fn mod_values(ctx: &Context, v1: &mut Value, v2: &mut Value) -> Result<Value> {
    let v1 = v1.force(ctx)?;
    let v2 = v2.force(ctx)?;

    match (v1, v2) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a & b)),
        _ => unreachable!(),
    }
}

pub fn and_values(ctx: &Context, v1: &mut Value, v2: &mut Value) -> Result<Value> {
    let v1 = v1.force(ctx)?;
    let v2 = v2.force(ctx)?;

    match (v1, v2) {
        (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(a && b)),
        _ => unreachable!(),
    }
}

pub fn or_values(ctx: &Context, v1: &mut Value, v2: &mut Value) -> Result<Value> {
    let v1 = v1.force(ctx)?;
    let v2 = v2.force(ctx)?;

    match (v1, v2) {
        (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(a || b)),
        _ => unreachable!(),
    }
}

pub fn cons_values(ctx: &Context, v1: &mut Value, v2: &mut Value) -> Result<Value> {
    let v1 = v1.force(ctx)?;
    let v2 = v2.force(ctx)?;

    match (v1, v2) {
        (Value::List(a), Value::List(b)) => Ok(Value::List(b.clone().push(Value::List(a.clone())))),
        (a, Value::List(b)) => Ok(Value::List(b.clone().push(a.clone()))),
        _ => unreachable!(),
    }
}

pub fn append_values(ctx: &Context, v1: &mut Value, v2: &mut Value) -> Result<Value> {
    let v1 = v1.force(ctx)?;
    let v2 = v2.force(ctx)?;

    match (v1, v2) {
        (Value::List(a), Value::List(b)) => Ok(Value::List(a.clone().append(b.clone()))),
        _ => unreachable!(),
    }
}

pub fn cmp_values(ctx: &Context, v1: &mut Value, v2: &mut Value) -> Result<Ordering> {
    let v1 = v1.force(ctx)?;
    let v2 = v2.force(ctx)?;

    match (v1, v2) {
        (Value::Integer(a), Value::Integer(b)) => Ok(a.cmp(&b)),
        (Value::Boolean(a), Value::Boolean(b)) => Ok(a.cmp(&b)),
        (Value::Char(a), Value::Char(b)) => Ok(a.cmp(&b)),
        (Value::List(a), Value::List(b)) => {
            let mut l1 = a;
            let mut l2 = b;
            loop {
                match (l1.head(), l2.head()) {
                    (None, None) => return Ok(Ordering::Equal),
                    (None, Some(_)) => return Ok(Ordering::Less),
                    (Some(_), None) => return Ok(Ordering::Greater),
                    (Some(mut a), Some(mut b)) => match cmp_values(ctx, &mut a, &mut b)? {
                        Ordering::Equal => {
                            l1 = l1.tail().unwrap_or_default();
                            l2 = l2.tail().unwrap_or_default();
                        }
                        ordering => return Ok(ordering),
                    },
                }
            }
        }
        (Value::Unit, Value::Unit) => Ok(Ordering::Equal),
        _ => unreachable!(),
    }
}

pub fn eq_values(ctx: &Context, v1: &mut Value, v2: &mut Value) -> Result<Value> {
    Ok(Value::Boolean(cmp_values(ctx, v1, v2)? == Ordering::Equal))
}

pub fn neq_values(ctx: &Context, v1: &mut Value, v2: &mut Value) -> Result<Value> {
    Ok(Value::Boolean(cmp_values(ctx, v1, v2)? != Ordering::Equal))
}

pub fn lt_values(ctx: &Context, v1: &mut Value, v2: &mut Value) -> Result<Value> {
    Ok(Value::Boolean(cmp_values(ctx, v1, v2)? == Ordering::Less))
}

pub fn le_values(ctx: &Context, v1: &mut Value, v2: &mut Value) -> Result<Value> {
    Ok(Value::Boolean(matches!(
        cmp_values(ctx, v1, v2)?,
        Ordering::Less | Ordering::Equal
    )))
}

pub fn gt_values(ctx: &Context, v1: &mut Value, v2: &mut Value) -> Result<Value> {
    Ok(Value::Boolean(
        cmp_values(ctx, v1, v2)? == Ordering::Greater,
    ))
}

pub fn ge_values(ctx: &Context, v1: &mut Value, v2: &mut Value) -> Result<Value> {
    Ok(Value::Boolean(matches!(
        cmp_values(ctx, v1, v2)?,
        Ordering::Greater | Ordering::Equal
    )))
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
            Value::Builtin(BuiltinKind::Value(v)) => Ok(v.borrow().clone()),
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
    Prefix(PrefixOp),
    Infix(InfixOp),
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
                    match ctx.get_node(param) {
                        Node::Expr(Expr::Identifier(id)) => {
                            result_stack.push_back(SharedValue::new(Value::Function {
                                param: id.clone(),
                                body,
                                env: Rc::new(RefCell::new(closure_env)),
                            }))
                        }
                        _ => unreachable!(),
                    }
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

                Node::Expr(Expr::Prefix { op, rhs }) => {
                    frame_stack.push_back(EvalFrame {
                        state: EvalState::Prefix(op),
                        ..frame.clone()
                    });

                    frame_stack.push_back(EvalFrame {
                        expr: rhs,
                        state: EvalState::Start,
                        ..frame
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
                let rhs = rhs.borrow();

                let res = match (op, &*rhs) {
                    (PrefixOp::Neg, Value::Integer(n)) => Value::Integer(-n),
                    (PrefixOp::Not, Value::Boolean(b)) => Value::Boolean(!b),
                    _ => unreachable!(),
                };

                result_stack.push_back(SharedValue::new(res));
            }

            EvalState::Infix(op) => {
                let lhs = result_stack.pop_back().unwrap();
                let mut lhs = lhs.borrow_mut();
                let rhs = result_stack.pop_back().unwrap();
                let mut rhs = rhs.borrow_mut();

                let res = match op {
                    InfixOp::Add => add_values(ctx, &mut lhs, &mut rhs)?,
                    InfixOp::Sub => sub_values(ctx, &mut lhs, &mut rhs)?,
                    InfixOp::Mul => mul_values(ctx, &mut lhs, &mut rhs)?,
                    InfixOp::Div => div_values(ctx, &mut lhs, &mut rhs)?,
                    InfixOp::Mod => mod_values(ctx, &mut lhs, &mut rhs)?,
                    InfixOp::Eq => eq_values(ctx, &mut lhs, &mut rhs)?,
                    InfixOp::Neq => neq_values(ctx, &mut lhs, &mut rhs)?,
                    InfixOp::Lt => lt_values(ctx, &mut lhs, &mut rhs)?,
                    InfixOp::Lte => le_values(ctx, &mut lhs, &mut rhs)?,
                    InfixOp::Gt => gt_values(ctx, &mut lhs, &mut rhs)?,
                    InfixOp::Gte => ge_values(ctx, &mut lhs, &mut rhs)?,
                    InfixOp::And => and_values(ctx, &mut lhs, &mut rhs)?,
                    InfixOp::Or => or_values(ctx, &mut lhs, &mut rhs)?,
                    InfixOp::Cons => cons_values(ctx, &mut lhs, &mut rhs)?,
                    InfixOp::Append => append_values(ctx, &mut lhs, &mut rhs)?,
                };

                result_stack.push_back(SharedValue::new(res));
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
                        result_stack.push_back(f(ctx, arg.clone(), *ctx.get_span(frame.expr))?);
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
                        let func = match ctx.get_node(*param) {
                            Node::Expr(Expr::Identifier(id)) => Value::Function {
                                param: id.clone(),
                                body: *body,
                                env: closure_env.clone(),
                            },
                            _ => unreachable!(),
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
