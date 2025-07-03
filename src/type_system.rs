use core::fmt;
use std::collections::HashMap;
use std::result;

use crate::{
    ast::{self, Expr, InfixOp, PrefixOp, Stmt},
    span::Spanned,
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0}: type mismatch expected '{1}' but found '{3}'")]
    TypeMismatch(Span, Type, Span, Type),

    #[error("{0}: infinite type '{1}'")]
    InfiniteType(Span, Type),

    #[error("{0}: unkown variable '{1}'")]
    UnknownIdentifier(Span, String),
}

type Result<T> = result::Result<T, Box<Error>>;

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Int,
    Bool,
    Var(usize),
    Fun(Box<Spanned<Type>>, Box<Spanned<Type>>),
    Poly(Vec<usize>, Box<Spanned<Type>>),
}

impl Type {
    fn fmt(&self, f: &mut fmt::Formatter, mapping: &HashMap<usize, usize>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "Unit"),

            Type::Int => write!(f, "Int"),

            Type::Bool => write!(f, "Bool"),

            Type::Var(n) => write!(f, "t{}", mapping.get(n).unwrap_or(n)),

            Type::Fun(l, r) => {
                l.value.fmt(f, mapping)?;
                write!(f, " -> ")?;
                r.value.fmt(f, mapping)
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
                body.value
                    .fmt(f, &vars.iter().enumerate().map(|(i, &v)| (v, i)).collect())
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
pub struct Typed<T> {
    pub value: T,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Env {
    vars: HashMap<String, Spanned<Type>>,
    counter: usize,
    substitutions: HashMap<usize, Spanned<Type>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            vars: HashMap::new(),
            counter: 0,
            substitutions: HashMap::new(),
        }
    }

    fn fresh_var(&mut self) -> Type {
        let var = Type::Var(self.counter);
        self.counter += 1;
        var
    }

    pub fn generalize(&self, ty: &Spanned<Type>) -> Spanned<Type> {
        let mut free_vars = Vec::new();
        Self::collect_free_vars(ty, &mut free_vars, &mut Vec::new());

        free_vars.retain(|v| !self.substitutions.contains_key(v));

        if free_vars.is_empty() {
            return ty.clone();
        }

        free_vars.sort();
        free_vars.dedup();

        Spanned::new(Type::Poly(free_vars, Box::new(ty.clone())), ty.span)
    }

    fn collect_free_vars(
        ty: &Spanned<Type>,
        free_vars: &mut Vec<usize>,
        bound_vars: &mut Vec<usize>,
    ) {
        match &ty.value {
            Type::Var(n) => {
                if !bound_vars.contains(n) {
                    free_vars.push(*n);
                }
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

    pub fn instantiate(&mut self, ty: &Spanned<Type>) -> Spanned<Type> {
        let mut substitutions = HashMap::new();
        self.instantiate_helper(ty, &mut substitutions)
    }

    fn instantiate_helper(
        &mut self,
        ty: &Spanned<Type>,
        substitutions: &mut HashMap<usize, Type>,
    ) -> Spanned<Type> {
        match &ty.value {
            Type::Var(n) => Spanned::new(
                substitutions.get(n).cloned().unwrap_or(Type::Var(*n)),
                ty.span,
            ),

            Type::Fun(l, r) => Spanned::new(
                Type::Fun(
                    Box::new(self.instantiate_helper(l, substitutions)),
                    Box::new(self.instantiate_helper(r, substitutions)),
                ),
                ty.span,
            ),

            Type::Poly(vars, body) => {
                for var in vars {
                    substitutions.insert(*var, self.fresh_var());
                }
                self.instantiate_helper(body, substitutions)
            }

            _ => ty.clone(),
        }
    }

    fn substitute(&self, ty: &Spanned<Type>) -> Spanned<Type> {
        match &ty.value {
            Type::Var(n) => {
                if let Some(sub) = self.substitutions.get(n) {
                    self.substitute(sub)
                } else {
                    ty.clone()
                }
            }

            Type::Fun(l, r) => Spanned::new(
                Type::Fun(Box::new(self.substitute(l)), Box::new(self.substitute(r))),
                ty.span,
            ),

            Type::Poly(vars, body) => {
                let mut new_subs = self.substitutions.clone();
                for var in vars {
                    new_subs.remove(var);
                }
                let new_env = Env {
                    vars: self.vars.clone(),
                    counter: self.counter,
                    substitutions: new_subs,
                };
                Spanned::new(
                    Type::Poly(vars.clone(), Box::new(new_env.substitute(body))),
                    ty.span,
                )
            }
            _ => ty.clone(),
        }
    }

    fn unify(&mut self, t1: &Spanned<Type>, t2: &Spanned<Type>) -> Result<()> {
        let t1 = self.substitute(t1);
        let t2 = self.substitute(t2);

        match (&t1.value, &t2.value) {
            (Type::Int, Type::Int) | (Type::Bool, Type::Bool) | (Type::Unit, Type::Unit) => Ok(()),

            (Type::Var(a), Type::Var(b)) if a == b => Ok(()),

            (Type::Fun(l1, r1), Type::Fun(l2, r2)) => {
                self.unify(l1, l2)?;
                self.unify(r1, r2)
            }

            (Type::Var(a), _) => {
                Self::occurs_check(*a, &t2)?;
                self.substitutions.insert(*a, t2.clone());
                Ok(())
            }

            (_, Type::Var(a)) => {
                Self::occurs_check(*a, &t1)?;
                self.substitutions.insert(*a, t1.clone());
                Ok(())
            }

            _ => Err(Box::new(Error::TypeMismatch(
                t1.span, t1.value, t2.span, t2.value,
            ))),
        }
    }

    fn occurs_check(var: usize, ty: &Spanned<Type>) -> Result<()> {
        match &ty.value {
            Type::Var(a) if *a == var => {
                Err(Box::new(Error::InfiniteType(ty.span, ty.value.clone())))
            }

            Type::Fun(l, r) => {
                Self::occurs_check(var, l)?;
                Self::occurs_check(var, r)
            }

            Type::Poly(vars, body) if !vars.contains(&var) => Self::occurs_check(var, body),

            _ => Ok(()),
        }
    }
}

pub fn infer_expr(env: &mut Env, expr: &Spanned<Expr>) -> Result<Spanned<Type>> {
    let ty = match &expr.value {
        Expr::Unit => Spanned::new(Type::Unit, expr.span),

        Expr::Integer(_) => Spanned::new(Type::Int, expr.span),

        Expr::Boolean(_) => Spanned::new(Type::Bool, expr.span),

        Expr::Identifier(name) => {
            let ty = env
                .vars
                .get(&name.clone())
                .cloned()
                .ok_or(Error::UnknownIdentifier(expr.span, name.clone()))?;

            env.instantiate(&ty)
        }

        Expr::Condition { cond, then, alt } => {
            let cond_ty = infer_expr(env, cond)?;

            env.unify(&cond_ty, &Spanned::new(Type::Bool, cond_ty.span))?;

            let then_ty = infer_expr(env, then)?;
            let alt_ty = infer_expr(env, alt)?;

            env.unify(&then_ty, &alt_ty)?;

            then_ty
        }

        Expr::Prefix { op, rhs } => {
            let rhs_ty = infer_expr(env, rhs)?;
            match op {
                PrefixOp::Neg => {
                    env.unify(&rhs_ty, &Spanned::new(Type::Int, rhs_ty.span))?;
                    Spanned::new(Type::Int, expr.span)
                }
            }
        }

        Expr::Infix { lhs, op, rhs } => {
            let lhs_ty = infer_expr(env, lhs)?;
            let rhs_ty = infer_expr(env, rhs)?;

            match op {
                InfixOp::Add | InfixOp::Sub | InfixOp::Mul | InfixOp::Div | InfixOp::Mod => {
                    env.unify(&lhs_ty, &Spanned::new(Type::Int, lhs_ty.span))?;
                    env.unify(&rhs_ty, &Spanned::new(Type::Int, rhs_ty.span))?;
                    Spanned::new(Type::Int, expr.span)
                }

                InfixOp::Eq => {
                    env.unify(&lhs_ty, &rhs_ty)?;
                    Spanned::new(Type::Bool, expr.span)
                }
            }
        }

        Expr::Lambda { param, body } => {
            let param_ty = Spanned::new(env.fresh_var(), param.span);
            env.vars.insert(param.value.clone(), param_ty.clone());
            let body_ty = infer_expr(env, body)?;
            Spanned::new(Type::Fun(Box::new(param_ty), Box::new(body_ty)), expr.span)
        }

        Expr::Application { func, arg } => {
            let func_ty = infer_expr(env, func)?;
            let arg_ty = infer_expr(env, arg)?;
            let ty = Spanned::new(env.fresh_var(), expr.span);
            let span = arg_ty.span.merge(&ty.span);
            env.unify(
                &func_ty,
                &Spanned::new(Type::Fun(Box::new(arg_ty), Box::new(ty.clone())), span),
            )?;

            ty
        }
    };

    Ok(env.substitute(&ty))
}

pub fn infer(ast: &[Spanned<Stmt>]) -> Result<Vec<Typed<Spanned<Stmt>>>> {
    let mut typed_ast = vec![];
    let mut env = Env::new();

    for node in ast {
        match &node.value {
            ast::Stmt::Let { name, expr } => {
                let ty = Spanned::new(env.fresh_var(), node.span);
                env.vars.insert(name.clone(), ty.clone());

                let expr_ty = infer_expr(&mut env, expr)?;

                let generalized_ty = env.generalize(&expr_ty);
                env.vars.insert(name.clone(), generalized_ty.clone());

                typed_ast.push(Typed {
                    value: node.clone(),
                    ty: generalized_ty.value,
                });
            }

            ast::Stmt::Expr(expr) => {
                let expr_ty = infer_expr(&mut env, expr)?;
                let generalized_ty = env.generalize(&expr_ty);
                typed_ast.push(Typed {
                    value: node.clone(),
                    ty: generalized_ty.value,
                });
            }
        }
    }

    Ok(typed_ast)
}
