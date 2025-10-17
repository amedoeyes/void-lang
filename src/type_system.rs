use core::fmt;
use std::hash::Hash;
use std::result;

use fxhash::{FxHashMap, FxHashSet};
use itertools::Itertools;

use crate::context::{Context, Node, NodeId};
use crate::{expr::Expr, span::Span};

#[macro_export]
macro_rules! ty  {
    (Int) => { Type::Int };
    (Char) => { Type::Char };
    (Bool) => { Type::Bool };
    (()) => { Type::Unit };

    ([$inner:tt]) => {
        Type::List(Box::new(ty!($inner)))
    };

    ($param:tt -> $body:tt) => {
        Type::Fun(Box::new(ty!($param)), Box::new(ty!($body)))
    };

    ($param:tt -> $body:tt -> $($rest:tt)*) => {
        Type::Fun(Box::new(ty!($param)), Box::new(ty!($body -> $($rest)*)))
    };

    ($var:literal) => {
        Type::Var($var)
    };

    (($($inner:tt)*)) => {
        ty!($($inner)*)
    };

    (forall $($vars:literal)* . ($($class:tt $var:literal),*) => $($body:tt)*) => {
        Type::Poly(
            [$($vars),*].into_iter().collect::<FxHashSet<usize>>(),
            Vec::from([$(Constraint { class: TypeClass::$class, ty: Type::Var($var), span: Span::default() }),*]),
            Box::new(ty!($($body)*))
        )
    };

    (forall $($vars:literal)* . $($body:tt)*) => {
        Type::Poly(
            [$($vars),*].into_iter().collect::<FxHashSet<usize>>(),
            Vec::new(),
            Box::new(ty!($($body)*))
        )
    };
}

#[derive(Debug)]
pub enum Error {
    TypeMismatch(String, String, Span),
    InfiniteType(String, Span),
    UnknownIdentifier(String, Span),
    UnknownOperator(String, Span),
    NoInstance(String, String, Span),
}

type Result<T> = result::Result<T, Error>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeClass {
    Eq,
    Ord,
    Num,
}

impl fmt::Display for TypeClass {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeClass::Eq => write!(f, "Eq"),
            TypeClass::Ord => write!(f, "Ord"),
            TypeClass::Num => write!(f, "Num"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constraint {
    pub class: TypeClass,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum Type {
    #[default]
    Unit,
    Int,
    Char,
    Bool,
    Var(usize),
    List(Box<Type>),
    Fun(Box<Type>, Box<Type>),
    Poly(FxHashSet<usize>, Vec<Constraint>, Box<Type>),
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
            Type::List(item) => item.collect_type_vars(vars),
            Type::Fun(param, body) => {
                param.collect_type_vars(vars);
                body.collect_type_vars(vars);
            }
            Type::Poly(_, _, body) => body.collect_type_vars(vars),
            _ => {}
        }
    }

    fn fmt(&self, f: &mut fmt::Formatter, var_order_map: &FxHashMap<usize, usize>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "Unit"),
            Type::Int => write!(f, "Int"),
            Type::Char => write!(f, "Char"),
            Type::Bool => write!(f, "Bool"),
            Type::Var(n) => write!(f, "t{}", var_order_map.get(n).unwrap_or(n)),
            Type::List(item) => {
                write!(f, "[")?;
                item.fmt(f, var_order_map)?;
                write!(f, "]")
            }
            Type::Fun(param, body) => {
                match param.as_ref() {
                    Type::Fun(_, _) => {
                        write!(f, "(")?;
                        param.fmt(f, var_order_map)?;
                        write!(f, ")")?;
                    }
                    _ => param.fmt(f, var_order_map)?,
                }
                write!(f, " -> ")?;
                body.fmt(f, var_order_map)
            }
            Type::Poly(vars, constraints, body) => {
                let var_order_map = vars
                    .iter()
                    .sorted()
                    .enumerate()
                    .map(|(i, v)| (*v, i + 1))
                    .collect::<FxHashMap<_, _>>();

                write!(
                    f,
                    "forall {} . ",
                    vars.iter()
                        .sorted()
                        .map(|v| format!("t{}", var_order_map.get(v).unwrap()))
                        .collect::<Vec<String>>()
                        .join(" ")
                )?;

                let constraints = constraints
                    .iter()
                    .filter_map(|c| match c.ty {
                        Type::Var(_) => Some((&c.class, c.ty.clone())),
                        _ => None,
                    })
                    .sorted_by_key(|(class, _)| **class)
                    .dedup()
                    .collect::<Vec<_>>();

                if !constraints.is_empty() {
                    write!(
                        f,
                        "({}) => ",
                        constraints
                            .iter()
                            .map(|c| format!(
                                "{} t{}",
                                c.0,
                                if let Type::Var(n) = c.1 {
                                    *var_order_map.get(&n).unwrap()
                                } else {
                                    unreachable!()
                                }
                            ))
                            .collect::<Vec<String>>()
                            .join(", ")
                    )?;
                }

                body.fmt(f, &var_order_map)
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
    counter: usize,
    substitutions: FxHashMap<usize, Type>,
    instances: FxHashMap<TypeClass, Vec<Type>>,
    constraints: Vec<Constraint>,
}

impl Env {
    fn new() -> Self {
        let mut instances = FxHashMap::default();
        instances.insert(
            TypeClass::Eq,
            Vec::from([
                ty!(Int),
                ty!(Bool),
                ty!(Char),
                ty!(()),
                ty!([Int]),
                ty!([Bool]),
                ty!([Char]),
                ty!([()]),
            ]),
        );
        instances.insert(
            TypeClass::Ord,
            Vec::from([
                ty!(Int),
                ty!(Bool),
                ty!(Char),
                ty!([Int]),
                ty!([Bool]),
                ty!([Char]),
            ]),
        );
        instances.insert(TypeClass::Num, Vec::from([ty!(Int)]));

        Env {
            counter: 0,
            substitutions: FxHashMap::default(),
            instances,
            constraints: Vec::new(),
        }
    }

    fn fresh_var(&mut self) -> Type {
        let var = Type::Var(self.counter);
        self.counter += 1;
        var
    }

    fn solve_constraints(&self) -> Result<()> {
        self.constraints.iter().try_for_each(|cons| {
            let cons_ty = self.substitute(&cons.ty);
            if self.check_instance(cons.class, &cons_ty) {
                Ok(())
            } else {
                Err(Error::NoInstance(
                    cons.class.to_string(),
                    cons_ty.to_string(),
                    cons.span,
                ))
            }
        })
    }

    fn check_instance(&self, class: TypeClass, ty: &Type) -> bool {
        match ty {
            Type::Var(_) => true,
            Type::List(inner) => self.check_instance(class, inner),
            _ => self
                .instances
                .get(&class)
                .is_some_and(|instances| instances.contains(ty)),
        }
    }

    fn generalize(&self, ty: &Type) -> Type {
        let type_vars = ty.type_vars();
        if type_vars.is_empty() {
            ty.clone()
        } else {
            let constraints = self
                .constraints
                .iter()
                .filter_map(|constraint| {
                    let cons_ty = self.substitute(&constraint.ty);
                    if cons_ty.type_vars().is_subset(&type_vars) {
                        Some(Constraint {
                            class: constraint.class,
                            ty: cons_ty,
                            span: constraint.span,
                        })
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();
            Type::Poly(type_vars, constraints, Box::new(ty.clone()))
        }
    }

    fn instantiate(&mut self, ty: Type, span: Span) -> Type {
        let mut substitutions = FxHashMap::default();
        self.instantiate_helper(ty, span, &mut substitutions)
    }

    fn instantiate_helper(
        &mut self,
        ty: Type,
        span: Span,
        substitutions: &mut FxHashMap<usize, Type>,
    ) -> Type {
        match ty {
            Type::Poly(vars, constraints, body) => {
                for var in vars {
                    substitutions.insert(var, self.fresh_var());
                }

                for cons in constraints {
                    let cons_ty = self.instantiate_helper(cons.ty, span, substitutions);
                    self.constraints.push(Constraint {
                        class: cons.class,
                        ty: cons_ty,
                        span,
                    });
                }

                self.instantiate_helper(*body, span, substitutions)
            }
            Type::Var(n) => substitutions.get(&n).cloned().unwrap_or(ty),
            Type::List(item) => Type::List(Box::new(self.instantiate_helper(
                *item,
                span,
                substitutions,
            ))),
            Type::Fun(param, body) => Type::Fun(
                Box::new(self.instantiate_helper(*param, span, substitutions)),
                Box::new(self.instantiate_helper(*body, span, substitutions)),
            ),
            _ => ty,
        }
    }

    fn substitute(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(n) => self
                .substitutions
                .get(n)
                .map(|ty| self.substitute(ty))
                .unwrap_or(ty.clone()),
            Type::List(item) => Type::List(Box::new(self.substitute(item))),
            Type::Fun(param, body) => Type::Fun(
                Box::new(self.substitute(param)),
                Box::new(self.substitute(body)),
            ),
            _ => ty.clone(),
        }
    }

    fn unify(&mut self, ty1: &Type, ty2: &Type, span: Span) -> Result<()> {
        match (self.substitute(ty1), self.substitute(ty2)) {
            (Type::Int, Type::Int)
            | (Type::Bool, Type::Bool)
            | (Type::Unit, Type::Unit)
            | (Type::Char, Type::Char) => Ok(()),
            (Type::Var(a), Type::Var(b)) if a == b => Ok(()),
            (Type::List(a), Type::List(b)) => self.unify(a.as_ref(), b.as_ref(), span),
            (Type::Fun(p1, b1), Type::Fun(p2, b2)) => {
                self.unify(p1.as_ref(), p2.as_ref(), span)?;
                self.unify(b1.as_ref(), b2.as_ref(), span)
            }
            (Type::Var(a), b) => {
                self.occurs_check(a, &b, span)?;
                self.substitutions.insert(a, b);
                Ok(())
            }
            (a, Type::Var(b)) => {
                self.occurs_check(b, &a, span)?;
                self.substitutions.insert(b, a);
                Ok(())
            }
            (a, b) => Err(Error::TypeMismatch(
                self.generalize(&a).to_string(),
                self.generalize(&b).to_string(),
                span,
            )),
        }
    }

    fn occurs_check(&self, var: usize, ty: &Type, span: Span) -> Result<()> {
        match &ty {
            Type::Var(n) if *n == var => {
                Err(Error::InfiniteType(self.generalize(ty).to_string(), span))
            }
            Type::List(item) => self.occurs_check(var, item, span),
            Type::Fun(param, body) => {
                self.occurs_check(var, param, span)?;
                self.occurs_check(var, body, span)
            }
            Type::Poly(vars, _, body) if !vars.contains(&var) => self.occurs_check(var, body, span),
            _ => Ok(()),
        }
    }
}

fn infer_expr(
    ctx: &mut Context,
    env: &mut Env,
    env_vars: &FxHashMap<String, Type>,
    expr: NodeId,
) -> Result<Type> {
    let ty = match ctx.get_node(expr).clone() {
        Node::Expr(Expr::Unit) => Type::Unit,
        Node::Expr(Expr::Integer(_)) => Type::Int,
        Node::Expr(Expr::Char(_)) => Type::Char,
        Node::Expr(Expr::Boolean(_)) => Type::Bool,
        Node::Expr(Expr::Nil) => Type::List(Box::new(env.fresh_var())),
        Node::Expr(Expr::Cons { head, tail }) => {
            let head_ty = infer_expr(ctx, env, env_vars, head)?;
            let mut tail = tail;

            while let Node::Expr(Expr::Cons {
                head: next_head,
                tail: next_tail,
            }) = ctx.get_node(tail).clone()
            {
                let next_head_ty = infer_expr(ctx, env, env_vars, next_head)?;
                env.unify(&head_ty, &next_head_ty, ctx.get_span(next_head))?;
                tail = next_tail;
            }

            Type::List(Box::new(head_ty))
        }
        Node::Expr(Expr::Identifier(name)) => env.instantiate(
            env_vars
                .get(&name)
                .ok_or(Error::UnknownIdentifier(name, ctx.get_span(expr)))?
                .clone(),
            ctx.get_span(expr),
        ),
        Node::Expr(Expr::Condition { cond, then, alt }) => {
            let cond_ty = infer_expr(ctx, env, env_vars, cond)?;
            let then_ty = infer_expr(ctx, env, env_vars, then)?;
            let alt_ty = infer_expr(ctx, env, env_vars, alt)?;
            env.unify(&Type::Bool, &cond_ty, ctx.get_span(cond))?;
            env.unify(&then_ty, &alt_ty, ctx.get_span(alt))?;
            alt_ty
        }
        Node::Expr(Expr::Infix { lhs, op, rhs }) => {
            let lhs_ty = infer_expr(ctx, env, env_vars, lhs)?;
            let rhs_ty = infer_expr(ctx, env, env_vars, rhs)?;

            let op_ty = env.instantiate(
                env_vars
                    .get(&op)
                    .ok_or(Error::UnknownOperator(op.clone(), ctx.get_span(expr)))?
                    .clone(),
                ctx.get_span(expr),
            );

            let res_ty = env.fresh_var();
            env.unify(
                &Type::Fun(
                    Box::new(lhs_ty.clone()),
                    Box::new(Type::Fun(
                        Box::new(rhs_ty.clone()),
                        Box::new(res_ty.clone()),
                    )),
                ),
                &op_ty,
                ctx.get_span(expr),
            )?;

            res_ty
        }
        Node::Expr(Expr::Lambda { param, body }) => {
            let param_ty = env.fresh_var();
            let mut new_env_vars = env_vars.clone();
            new_env_vars.insert(param, param_ty.clone());
            let body_ty = infer_expr(ctx, env, &new_env_vars, body)?;
            Type::Fun(Box::new(param_ty), Box::new(body_ty))
        }
        Node::Expr(Expr::Application { func, arg }) => {
            let func_ty = infer_expr(ctx, env, env_vars, func)?;
            let arg_ty = infer_expr(ctx, env, env_vars, arg)?;
            let ret_ty = env.fresh_var();
            env.unify(
                &func_ty,
                &Type::Fun(Box::new(arg_ty), Box::new(ret_ty.clone())),
                ctx.get_span(arg),
            )?;
            ret_ty
        }
        _ => unreachable!(),
    };

    let ty = env.substitute(&ty);
    ctx.set_type(expr, ty.clone());
    Ok(ty)
}

pub fn infer(ctx: &mut Context, nodes: &[NodeId]) -> Result<()> {
    let mut env = Env::new();
    let mut env_vars = FxHashMap::default();

    for (name, builtin) in ctx.builtins() {
        env_vars.insert(name.clone(), env.generalize(&builtin.ty));
    }

    for node in nodes {
        match ctx.get_node(*node).clone() {
            Node::Expr(_) => {
                let expr_ty = infer_expr(ctx, &mut env, &env_vars, *node)?;
                ctx.set_type(*node, env.generalize(&env.substitute(&expr_ty)));
            }
            Node::Bind(name, expr) => {
                let ty = env.fresh_var();
                env_vars.insert(name.clone(), ty);
                let expr_ty = infer_expr(ctx, &mut env, &env_vars, expr)?;
                let expr_ty = env.generalize(&env.substitute(&expr_ty));
                env_vars.insert(name.clone(), expr_ty.clone());
                ctx.set_type(expr, expr_ty.clone());
                ctx.set_type(*node, expr_ty);
            }
        }
    }

    env.solve_constraints()?;

    Ok(())
}
