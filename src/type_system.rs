use core::fmt;
use std::collections::HashMap;
use std::result;

use crate::builtin::Builtins;
use crate::context::{Context, Node, NodeId};
use crate::{
    expr::{Expr, InfixOp, PrefixOp},
    span::Span,
};

#[derive(Debug)]
pub enum Error {
    TypeMismatch((String, Span), (String, Span)),
    InfiniteType(String, Span),
    UnknownIdentifier(String, Span),
}

type Result<T> = result::Result<T, Error>;

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Int,
    Bool,
    Var(usize),
    List(Box<Type>),
    Fun(Box<Type>, Box<Type>),
    Poly(Vec<usize>, Box<Type>),
}

impl Type {
    fn fmt(&self, f: &mut fmt::Formatter, mapping: &HashMap<usize, usize>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "Unit"),

            Type::Int => write!(f, "Int"),

            Type::Bool => write!(f, "Bool"),

            Type::Var(n) => write!(f, "t{}", mapping.get(n).unwrap_or(n)),

            Type::List(e) => {
                write!(f, "[")?;
                e.fmt(f, mapping)?;
                write!(f, "]")
            }

            Type::Fun(l, r) => {
                l.fmt(f, mapping)?;
                write!(f, " -> ")?;
                r.fmt(f, mapping)
            }

            Type::Poly(vars, body) => {
                write!(
                    f,
                    "forall {} . ",
                    vars.iter()
                        .enumerate()
                        .map(|(i, _)| format!("t{i}"))
                        .collect::<Vec<String>>()
                        .join(" ")
                )?;
                body.fmt(f, &vars.iter().enumerate().map(|(i, &v)| (v, i)).collect())
            }
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt(f, &HashMap::new())
    }
}

#[derive(Debug, Clone)]
struct Env {
    vars: HashMap<String, Type>,
    counter: usize,
    substitutions: HashMap<usize, Type>,
}

impl Env {
    fn new() -> Self {
        Env {
            vars: HashMap::new(),
            counter: 3,
            substitutions: HashMap::new(),
        }
    }

    fn fresh_var(&mut self) -> Type {
        let var = Type::Var(self.counter);
        self.counter += 1;
        var
    }

    fn generalize(&self, ty: Type) -> Type {
        let mut free_vars = Vec::new();
        Self::collect_free_vars(&ty, &mut free_vars, &mut Vec::new());

        free_vars.retain(|v| !self.substitutions.contains_key(v));

        if free_vars.is_empty() {
            return ty;
        }

        free_vars.sort();
        free_vars.dedup();

        Type::Poly(free_vars, Box::new(ty))
    }

    fn collect_free_vars(ty: &Type, free_vars: &mut Vec<usize>, bound_vars: &mut Vec<usize>) {
        match &ty {
            Type::Var(n) => {
                if !bound_vars.contains(n) {
                    free_vars.push(*n);
                }
            }

            Type::List(e) => {
                Self::collect_free_vars(e, free_vars, bound_vars);
            }

            Type::Fun(l, r) => {
                Self::collect_free_vars(l, free_vars, bound_vars);
                Self::collect_free_vars(r, free_vars, bound_vars);
            }

            Type::Poly(vars, body) => {
                let mut new_bound = bound_vars.clone();
                new_bound.extend(vars);
                Self::collect_free_vars(body, free_vars, &mut new_bound);
            }

            _ => {}
        }
    }

    fn instantiate(&mut self, ty: Type) -> Type {
        let mut substitutions = HashMap::new();
        self.instantiate_helper(ty, &mut substitutions)
    }

    fn instantiate_helper(&mut self, ty: Type, substitutions: &mut HashMap<usize, Type>) -> Type {
        match ty {
            Type::Var(n) => substitutions.get(&n).cloned().unwrap_or(Type::Var(n)),

            Type::List(e) => Type::List(Box::new(self.instantiate_helper(*e, substitutions))),

            Type::Fun(l, r) => Type::Fun(
                Box::new(self.instantiate_helper(*l, substitutions)),
                Box::new(self.instantiate_helper(*r, substitutions)),
            ),

            Type::Poly(vars, body) => {
                for var in vars {
                    substitutions.insert(var, self.fresh_var());
                }
                self.instantiate_helper(*body, substitutions)
            }

            _ => ty,
        }
    }

    fn substitute(&self, ty: Type) -> Type {
        match ty {
            Type::Var(n) => {
                if let Some(sub) = self.substitutions.get(&n) {
                    self.substitute(sub.clone())
                } else {
                    ty
                }
            }

            Type::List(e) => Type::List(Box::new(self.substitute(*e))),

            Type::Fun(l, r) => {
                Type::Fun(Box::new(self.substitute(*l)), Box::new(self.substitute(*r)))
            }

            Type::Poly(vars, body) => {
                let mut new_subs = self.substitutions.clone();
                for var in &vars {
                    new_subs.remove(var);
                }
                let new_env = Env {
                    vars: self.vars.clone(),
                    counter: self.counter,
                    substitutions: new_subs,
                };
                Type::Poly(vars, Box::new(new_env.substitute(*body)))
            }
            _ => ty,
        }
    }

    fn unify(&mut self, t1: (Type, Span), t2: (Type, Span)) -> Result<()> {
        let (t1, s1) = t1;
        let (t2, s2) = t2;

        let t1 = self.substitute(t1);
        let t2 = self.substitute(t2);

        match (t1, t2) {
            (Type::Int, Type::Int) | (Type::Bool, Type::Bool) | (Type::Unit, Type::Unit) => Ok(()),

            (Type::Var(a), Type::Var(b)) if a == b => Ok(()),

            (Type::List(a), Type::List(b)) => self.unify((*a, s1), (*b, s2)),

            (Type::Fun(l1, r1), Type::Fun(l2, r2)) => {
                self.unify((*l1, s1), (*l2, s2))?;
                self.unify((*r1, s1), (*r2, s2))
            }

            (Type::Var(a), b) => {
                self.occurs_check(a, &b, s2)?;
                self.substitutions.insert(a, b);
                Ok(())
            }

            (a, Type::Var(b)) => {
                self.occurs_check(b, &a, s1)?;
                self.substitutions.insert(b, a);
                Ok(())
            }

            (a, b) => Err(Error::TypeMismatch(
                (self.generalize(a).to_string(), s1),
                (self.generalize(b).to_string(), s2),
            )),
        }
    }

    fn occurs_check(&self, var: usize, ty: &Type, span: Span) -> Result<()> {
        match &ty {
            Type::Var(a) if *a == var => Err(Error::InfiniteType(
                self.generalize(ty.clone()).to_string(),
                span,
            )),

            Type::List(e) => self.occurs_check(var, e, span),

            Type::Fun(l, r) => {
                self.occurs_check(var, l, span)?;
                self.occurs_check(var, r, span)
            }

            Type::Poly(vars, body) if !vars.contains(&var) => self.occurs_check(var, body, span),

            _ => Ok(()),
        }
    }
}

fn infer_expr(ctx: &mut Context, env: &mut Env, expr: NodeId) -> Result<()> {
    let ty = match ctx.get_node(expr).clone() {
        Node::Expr(Expr::Unit) => Type::Unit,

        Node::Expr(Expr::Integer(_)) => Type::Int,

        Node::Expr(Expr::Boolean(_)) => Type::Bool,

        Node::Expr(Expr::Nil) => Type::List(Box::new(env.fresh_var())),

        Node::Expr(Expr::Cons { head, tail }) => {
            infer_expr(ctx, env, head)?;
            let head_ty = ctx.get_type(head).clone();
            let head_span = *ctx.get_span(head);

            let mut tail = tail;
            while let Node::Expr(Expr::Cons { head, tail: t }) = ctx.get_node(tail).clone() {
                infer_expr(ctx, env, head)?;
                let next_head_ty = ctx.get_type(head).clone();
                let next_head_span = *ctx.get_span(head);
                env.unify(
                    (head_ty.clone(), head_span),
                    (next_head_ty.clone(), next_head_span),
                )?;
                tail = t;
            }

            Type::List(Box::new(head_ty.clone()))
        }

        Node::Expr(Expr::Identifier(name)) => env.instantiate(
            env.vars
                .get(&name)
                .cloned()
                .ok_or(Error::UnknownIdentifier(name, *ctx.get_span(expr)))?,
        ),

        Node::Expr(Expr::Condition { cond, then, alt }) => {
            infer_expr(ctx, env, cond)?;
            let cond_ty = ctx.get_type(cond);
            let cond_span = ctx.get_span(cond);

            env.unify((Type::Bool, *cond_span), (cond_ty.clone(), *cond_span))?;

            infer_expr(ctx, env, then)?;
            infer_expr(ctx, env, alt)?;

            let then_ty = ctx.get_type(then);
            let alt_ty = ctx.get_type(alt);

            env.unify(
                (then_ty.clone(), *ctx.get_span(then)),
                (alt_ty.clone(), *ctx.get_span(alt)),
            )?;

            then_ty.clone()
        }

        Node::Expr(Expr::Prefix { op, rhs }) => {
            infer_expr(ctx, env, rhs)?;
            let rhs_ty = ctx.get_type(rhs);
            match op {
                PrefixOp::Neg => {
                    env.unify(
                        (Type::Int, *ctx.get_span(rhs)),
                        (rhs_ty.clone(), *ctx.get_span(rhs)),
                    )?;
                    Type::Int
                }

                PrefixOp::Not => {
                    env.unify(
                        (Type::Bool, *ctx.get_span(rhs)),
                        (rhs_ty.clone(), *ctx.get_span(rhs)),
                    )?;
                    Type::Bool
                }
            }
        }

        Node::Expr(Expr::Infix { lhs, op, rhs }) => {
            infer_expr(ctx, env, lhs)?;
            infer_expr(ctx, env, rhs)?;

            let lhs_ty = ctx.get_type(lhs);
            let rhs_ty = ctx.get_type(rhs);

            match op {
                InfixOp::Add | InfixOp::Sub | InfixOp::Mul | InfixOp::Div | InfixOp::Mod => {
                    env.unify(
                        (Type::Int, *ctx.get_span(lhs)),
                        (lhs_ty.clone(), *ctx.get_span(lhs)),
                    )?;
                    env.unify(
                        (Type::Int, *ctx.get_span(rhs)),
                        (rhs_ty.clone(), *ctx.get_span(rhs)),
                    )?;
                    Type::Int
                }

                InfixOp::Lt | InfixOp::Gt | InfixOp::Lte | InfixOp::Gte => {
                    env.unify(
                        (Type::Int, *ctx.get_span(lhs)),
                        (lhs_ty.clone(), *ctx.get_span(lhs)),
                    )?;
                    env.unify(
                        (Type::Int, *ctx.get_span(rhs)),
                        (rhs_ty.clone(), *ctx.get_span(rhs)),
                    )?;
                    Type::Bool
                }

                InfixOp::Eq | InfixOp::Neq => {
                    env.unify(
                        (lhs_ty.clone(), *ctx.get_span(lhs)),
                        (rhs_ty.clone(), *ctx.get_span(rhs)),
                    )?;
                    Type::Bool
                }

                InfixOp::And | InfixOp::Or => {
                    env.unify(
                        (Type::Bool, *ctx.get_span(lhs)),
                        (lhs_ty.clone(), *ctx.get_span(lhs)),
                    )?;
                    env.unify(
                        (Type::Bool, *ctx.get_span(rhs)),
                        (rhs_ty.clone(), *ctx.get_span(rhs)),
                    )?;
                    Type::Bool
                }

                InfixOp::Cons => {
                    env.unify(
                        (rhs_ty.clone(), *ctx.get_span(rhs)),
                        (Type::List(Box::new(lhs_ty.clone())), *ctx.get_span(lhs)),
                    )?;
                    rhs_ty.clone()
                }

                InfixOp::Append => {
                    env.unify(
                        (lhs_ty.clone(), *ctx.get_span(lhs)),
                        (rhs_ty.clone(), *ctx.get_span(rhs)),
                    )?;
                    lhs_ty.clone()
                }
            }
        }

        Node::Expr(Expr::Lambda { param, body }) => match ctx.get_node(param).clone() {
            Node::Expr(Expr::Identifier(id)) => {
                let param_ty = env.fresh_var();
                env.vars.insert(id.clone(), param_ty.clone());
                infer_expr(ctx, env, body)?;
                Type::Fun(Box::new(param_ty), Box::new(ctx.get_type(body).clone()))
            }
            _ => unreachable!(),
        },

        Node::Expr(Expr::Application { func, arg }) => {
            infer_expr(ctx, env, func)?;
            infer_expr(ctx, env, arg)?;
            let func_ty = ctx.get_type(func);
            let arg_ty = ctx.get_type(arg);
            let ty = env.fresh_var();
            env.unify(
                (func_ty.clone(), *ctx.get_span(func)),
                (
                    Type::Fun(Box::new(arg_ty.clone()), Box::new(ty.clone())),
                    *ctx.get_span(expr),
                ),
            )?;
            ty
        }

        _ => unreachable!(),
    };

    ctx.set_type(expr, env.substitute(ty));

    Ok(())
}

pub fn infer(ctx: &mut Context, builtins: &Builtins, nodes: &[NodeId]) -> Result<()> {
    let mut env = Env::new();

    for (name, builtin) in builtins {
        env.vars
            .insert(name.clone(), env.generalize(builtin.ty.clone()));
    }

    for node in nodes {
        match ctx.get_node(*node).clone() {
            Node::Expr(_) => {
                infer_expr(ctx, &mut env, *node)?;
                ctx.set_type(
                    *node,
                    env.generalize(env.substitute(ctx.get_type(*node).clone())),
                );
            }

            Node::Bind(name, expr) => {
                let ty = env.fresh_var();
                env.vars.insert(name.clone(), ty);
                infer_expr(ctx, &mut env, expr)?;
                let ty = env.generalize(env.substitute(ctx.get_type(expr).clone()));
                env.vars.insert(name.clone(), ty.clone());
                ctx.set_type(expr, ty.clone());
                ctx.set_type(*node, ty);
            }
        }
    }

    Ok(())
}
