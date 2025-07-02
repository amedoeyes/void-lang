use std::collections::HashMap;
use std::result;

use crate::{
    ast::{self, Expr, InfixOp, PrefixOp, Stmt},
    position::{Span, Spanned},
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("{0}: type mismatch between '{1}' and '{2}'")]
    TypeMismatch(Span, Type, Type),

    #[error("{0}: unkown variable '{1}'")]
    UnknownIdentifier(Span, String),
}

type Result<T> = result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Int,
    Bool,
    Var(usize),
    Fun(Box<Type>, Box<Type>),
    Poly(Vec<usize>, Box<Type>),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Unit => write!(f, "Unit"),
            Type::Int => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::Var(n) => write!(f, "t{n}"),
            Type::Fun(l, r) => write!(f, "{l} -> {r}"),
            Type::Poly(vars, body) => {
                write!(f, "forall ")?;
                for (i, var) in vars.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "t{var}")?;
                }
                write!(f, " . {body}")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Typed<T> {
    pub value: T,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct Env {
    vars: HashMap<String, Type>,
    counter: usize,
    substitutions: HashMap<usize, Type>,
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

    pub fn generalize(&self, ty: &Type) -> Type {
        let mut free_vars = Vec::new();
        Self::collect_free_vars(ty, &mut free_vars, &mut Vec::new());

        free_vars.retain(|v| !self.substitutions.contains_key(v));

        if free_vars.is_empty() {
            return ty.clone();
        }

        free_vars.sort();
        free_vars.dedup();

        Type::Poly(free_vars, Box::new(ty.clone()))
    }

    fn collect_free_vars(ty: &Type, free_vars: &mut Vec<usize>, bound_vars: &mut Vec<usize>) {
        match ty {
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

    pub fn instantiate(&mut self, ty: &Type) -> Type {
        let mut substitutions = HashMap::new();
        self.instantiate_helper(ty, &mut substitutions)
    }

    fn instantiate_helper(&mut self, ty: &Type, substitutions: &mut HashMap<usize, Type>) -> Type {
        match ty {
            Type::Poly(vars, body) => {
                for var in vars {
                    substitutions.insert(*var, self.fresh_var());
                }
                self.instantiate_helper(body, substitutions)
            }

            Type::Var(n) => substitutions.get(n).cloned().unwrap_or(Type::Var(*n)),

            Type::Fun(l, r) => Type::Fun(
                Box::new(self.instantiate_helper(l, substitutions)),
                Box::new(self.instantiate_helper(r, substitutions)),
            ),

            _ => ty.clone(),
        }
    }

    fn substitute(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(n) => {
                if let Some(sub) = self.substitutions.get(n) {
                    self.substitute(sub)
                } else {
                    ty.clone()
                }
            }

            Type::Fun(l, r) => {
                Type::Fun(Box::new(self.substitute(l)), Box::new(self.substitute(r)))
            }

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
                Type::Poly(vars.clone(), Box::new(new_env.substitute(body)))
            }
            _ => ty.clone(),
        }
    }

    fn unify(&mut self, t1: &Type, t2: &Type) -> result::Result<(), (Type, Type)> {
        let t1 = self.substitute(t1);
        let t2 = self.substitute(t2);

        match (&t1, &t2) {
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

            _ => Err((t1, t2)),
        }
    }

    fn occurs_check(var: usize, ty: &Type) -> result::Result<(), (Type, Type)> {
        match ty {
            Type::Var(a) if *a == var => Err((Type::Var(var), ty.clone())),

            Type::Fun(l, r) => {
                Self::occurs_check(var, l)?;
                Self::occurs_check(var, r)
            }

            Type::Poly(vars, body) if !vars.contains(&var) => Self::occurs_check(var, body),

            _ => Ok(()),
        }
    }
}

pub fn infer_expr(env: &mut Env, expr: &Spanned<Expr>) -> Result<Type> {
    let ty = match &expr.value {
        Expr::Unit => Ok(Type::Unit),

        Expr::Integer(_) => Ok(Type::Int),

        Expr::Boolean(_) => Ok(Type::Bool),

        Expr::Identifier(name) => {
            let ty = env
                .vars
                .get(&name.clone())
                .cloned()
                .ok_or(Error::UnknownIdentifier(expr.span, name.clone()))?;

            Ok(env.instantiate(&ty))
        }

        Expr::Condition { cond, then, alt } => {
            let cond_ty = infer_expr(env, cond)?;

            env.unify(&cond_ty, &Type::Bool)
                .map_err(|err| Error::TypeMismatch(cond.span, err.0, err.1))?;

            let then_ty = infer_expr(env, then)?;
            let alt_ty = infer_expr(env, alt)?;

            env.unify(&then_ty, &alt_ty)
                .map_err(|err| Error::TypeMismatch(alt.span, err.0, err.1))?;

            Ok(then_ty)
        }

        Expr::Prefix { op, rhs } => {
            let rhs_ty = infer_expr(env, rhs)?;
            match op {
                PrefixOp::Neg => {
                    env.unify(&rhs_ty, &Type::Int)
                        .map_err(|err| Error::TypeMismatch(rhs.span, err.0, err.1))?;
                    Ok(Type::Int)
                }
            }
        }

        Expr::Infix { lhs, op, rhs } => {
            let lhs_ty = infer_expr(env, lhs)?;
            let rhs_ty = infer_expr(env, rhs)?;

            match op {
                InfixOp::Add | InfixOp::Sub | InfixOp::Mul | InfixOp::Div | InfixOp::Mod => {
                    env.unify(&lhs_ty, &Type::Int)
                        .map_err(|err| Error::TypeMismatch(lhs.span, err.0, err.1))?;

                    env.unify(&rhs_ty, &Type::Int)
                        .map_err(|err| Error::TypeMismatch(rhs.span, err.0, err.1))?;

                    Ok(Type::Int)
                }

                InfixOp::Eq => {
                    env.unify(&lhs_ty, &rhs_ty)
                        .map_err(|err| Error::TypeMismatch(lhs.span, err.0, err.1))?;

                    Ok(Type::Bool)
                }
            }
        }

        Expr::Lambda { param, body } => {
            let param_ty = env.fresh_var();
            env.vars.insert(param.clone(), param_ty.clone());
            let body_ty = infer_expr(env, body)?;
            Ok(Type::Fun(Box::new(param_ty), Box::new(body_ty)))
        }

        Expr::Application { func, arg } => {
            let func_ty = infer_expr(env, func)?;
            let arg_ty = infer_expr(env, arg)?;
            let ty = env.fresh_var();

            env.unify(&func_ty, &Type::Fun(Box::new(arg_ty), Box::new(ty.clone())))
                .map_err(|err| Error::TypeMismatch(func.span, err.0, err.1))?;

            Ok(ty)
        }
    }?;

    Ok(env.substitute(&ty))
}

pub fn infer(prog: &[Spanned<Stmt>]) -> Result<Vec<Typed<Spanned<Stmt>>>> {
    let mut typed_prog = vec![];
    let mut env = Env::new();

    for stmt in prog {
        match &stmt.value {
            ast::Stmt::Let { name, expr } => {
                let ty = env.fresh_var();
                env.vars.insert(name.clone(), ty.clone());

                let expr_ty = infer_expr(&mut env, expr)?;

                let generalized_ty = env.generalize(&expr_ty);
                env.vars.insert(name.clone(), generalized_ty.clone());

                typed_prog.push(Typed {
                    value: stmt.clone(),
                    ty: generalized_ty,
                });
            }

            ast::Stmt::Expr(expr) => {
                let expr_ty = infer_expr(&mut env, expr)?;
                let generalized_ty = env.generalize(&expr_ty);
                typed_prog.push(Typed {
                    value: stmt.clone(),
                    ty: generalized_ty,
                });
            }
        }
    }

    Ok(typed_prog)
}
