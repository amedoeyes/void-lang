use core::fmt;
use std::result;

use fxhash::FxHashMap;

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
    Char,
    Bool,
    Var(usize),
    List(Box<Type>),
    Fun(Box<Type>, Box<Type>),
    Poly(FxHashSet<usize>, Box<Type>),
}

impl Type {
    fn type_vars(&self) -> FxHashSet<usize> {
        let mut vars = FxHashSet::default();
        self.collect_type_vars(&mut vars);
        vars
    }

    fn collect_type_vars(&self, vars: &mut FxHashSet<usize>) {
        match self {
            Type::Var(n) => {
                vars.insert(*n);
            }
            Type::List(ty) => ty.collect_type_vars(vars),
            Type::Fun(l, r) => {
                l.collect_type_vars(vars);
                r.collect_type_vars(vars);
            }
            Type::Poly(_, body) => body.collect_type_vars(vars),
            _ => {}
        }
    }

    fn instantiate(self) -> Type {
        match self {
            Type::Poly(vars, body) => {
                let mut substitutions = FxHashMap::default();
                for (i, var) in vars.iter().enumerate() {
                    substitutions.insert(*var, Type::Var(i));
                }
                Type::substitute_with(*body, &substitutions)
            }
            _ => self,
        }
    }

    fn substitute_with(ty: Type, substitutions: &FxHashMap<usize, Type>) -> Type {
        match ty {
            Type::Var(n) => substitutions.get(&n).cloned().unwrap_or(ty),
            Type::List(e) => Type::List(Box::new(Type::substitute_with(*e, substitutions))),
            Type::Fun(l, r) => Type::Fun(
                Box::new(Type::substitute_with(*l, substitutions)),
                Box::new(Type::substitute_with(*r, substitutions)),
            ),
            _ => ty,
        }
    }

    fn fmt(&self, f: &mut fmt::Formatter, mapping: &FxHashMap<usize, usize>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "Unit"),

            Type::Int => write!(f, "Int"),

            Type::Char => write!(f, "Char"),

            Type::Bool => write!(f, "Bool"),

            Type::Var(n) => write!(f, "t{}", mapping.get(n).unwrap_or(n)),

            Type::List(e) => {
                write!(f, "[")?;
                e.fmt(f, mapping)?;
                write!(f, "]")
            }

            Type::Fun(l, r) => {
                match l.as_ref() {
                    Type::Fun(_, _) => {
                        write!(f, "(")?;
                        l.fmt(f, mapping)?;
                        write!(f, ")")?;
                    }
                    _ => l.fmt(f, mapping)?,
                }
                write!(f, " -> ")?;
                r.fmt(f, mapping)
            }

            Type::Poly(vars, body) => {
                write!(
                    f,
                    "forall {} . ",
                    vars.iter()
                        .sorted()
                        .enumerate()
                        .map(|(i, _)| format!("t{i}"))
                        .collect::<Vec<String>>()
                        .join(" ")
                )?;
                body.fmt(
                    f,
                    &vars
                        .iter()
                        .sorted()
                        .enumerate()
                        .map(|(i, &v)| (v, i))
                        .collect(),
                )
            }
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt(f, &FxHashMap::default())
    }
}

#[derive(Debug, Clone)]
struct Env {
    vars: FxHashMap<String, Type>,
    counter: usize,
    substitutions: FxHashMap<usize, Type>,
}

impl Env {
    fn new() -> Self {
        Env {
            vars: FxHashMap::default(),
            counter: 3,
            substitutions: FxHashMap::default(),
        }
    }

    fn fresh_var(&mut self) -> Type {
        let var = Type::Var(self.counter);
        self.counter += 1;
        var
    }

    fn generalize(&self, ty: Type) -> Type {
        let type_vars = ty.type_vars();
        if type_vars.is_empty() {
            ty
        } else {
            Type::Poly(type_vars, Box::new(ty))
        }
    }

    fn instantiate(&mut self, ty: Type) -> Type {
        let mut substitutions = FxHashMap::default();
        self.instantiate_helper(ty, &mut substitutions)
    }

    fn instantiate_helper(&mut self, ty: Type, substitutions: &mut FxHashMap<usize, Type>) -> Type {
        match ty {
            Type::Poly(vars, body) => {
                for var in vars {
                    substitutions.insert(var, self.fresh_var());
                }
                self.instantiate_helper(*body, substitutions)
            }
            Type::Var(n) => substitutions.get(&n).cloned().unwrap_or(ty),
            Type::List(e) => Type::List(Box::new(self.instantiate_helper(*e, substitutions))),
            Type::Fun(l, r) => Type::Fun(
                Box::new(self.instantiate_helper(*l, substitutions)),
                Box::new(self.instantiate_helper(*r, substitutions)),
            ),
            _ => ty,
        }
    }

    fn substitute(&self, ty: Type) -> Type {
        match ty {
            Type::Var(n) => self
                .substitutions
                .get(&n)
                .map(|ty| self.substitute(ty.clone()))
                .unwrap_or(ty),
            Type::List(e) => Type::List(Box::new(self.substitute(*e))),
            Type::Fun(l, r) => {
                Type::Fun(Box::new(self.substitute(*l)), Box::new(self.substitute(*r)))
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
            (Type::Int, Type::Int)
            | (Type::Bool, Type::Bool)
            | (Type::Unit, Type::Unit)
            | (Type::Char, Type::Char) => Ok(()),

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

        Node::Expr(Expr::Char(_)) => Type::Char,

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
                        (Type::Int, *ctx.get_span(rhs)),
                        (lhs_ty.clone(), *ctx.get_span(lhs)),
                    )?;
                    env.unify(
                        (Type::Int, *ctx.get_span(lhs)),
                        (rhs_ty.clone(), *ctx.get_span(rhs)),
                    )?;
                    Type::Int
                }

                InfixOp::Lt | InfixOp::Gt | InfixOp::Lte | InfixOp::Gte => {
                    env.unify(
                        (lhs_ty.clone(), *ctx.get_span(lhs)),
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
                        (Type::Bool, *ctx.get_span(rhs)),
                        (lhs_ty.clone(), *ctx.get_span(lhs)),
                    )?;
                    env.unify(
                        (Type::Bool, *ctx.get_span(lhs)),
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
