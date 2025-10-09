use core::fmt;
use std::hash::Hash;
use std::result;

use fxhash::{FxHashMap, FxHashSet};
use itertools::Itertools;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
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
            Vec::from([Type::Int, Type::Bool, Type::Char, Type::Unit]),
        );
        instances.insert(
            TypeClass::Ord,
            Vec::from([Type::Int, Type::Bool, Type::Char]),
        );
        instances.insert(TypeClass::Num, Vec::from([Type::Int]));

        Env {
            counter: 1000,
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

    fn add_constraint(&mut self, class: TypeClass, ty: Type, span: Span) {
        self.constraints.push(Constraint { class, ty, span });
    }

    fn solve_constraints(&mut self) -> Result<()> {
        self.constraints.iter().try_for_each(|constraint| {
            let cons_ty = self.substitute(&constraint.ty);
            if self.check_instance(&constraint.class, &cons_ty) {
                Ok(())
            } else {
                Err(Error::NoInstance(
                    constraint.class.to_string(),
                    cons_ty.to_string(),
                    constraint.span,
                ))
            }
        })
    }

    fn check_instance(&self, class: &TypeClass, ty: &Type) -> bool {
        match ty {
            Type::Var(_) => true,
            Type::List(item) => match class {
                TypeClass::Eq | TypeClass::Ord => self.check_instance(class, item),
                _ => false,
            },
            _ => self
                .instances
                .get(class)
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

    fn instantiate(&mut self, ty: Type) -> Type {
        let mut substitutions = FxHashMap::default();
        self.instantiate_helper(ty, &mut substitutions)
    }

    fn instantiate_helper(&mut self, ty: Type, substitutions: &mut FxHashMap<usize, Type>) -> Type {
        match ty {
            Type::Poly(vars, constraints, body) => {
                for var in vars {
                    substitutions.insert(var, self.fresh_var());
                }

                for constraint in constraints {
                    let constrained_ty = self.instantiate_helper(constraint.ty, substitutions);
                    self.add_constraint(constraint.class, constrained_ty, constraint.span);
                }

                self.instantiate_helper(*body, substitutions)
            }
            Type::Var(n) => substitutions.get(&n).cloned().unwrap_or(ty),
            Type::List(item) => Type::List(Box::new(self.instantiate_helper(*item, substitutions))),
            Type::Fun(param, body) => Type::Fun(
                Box::new(self.instantiate_helper(*param, substitutions)),
                Box::new(self.instantiate_helper(*body, substitutions)),
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

    fn unify(&mut self, t1: (&Type, Span), t2: (&Type, Span)) -> Result<()> {
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
            (Type::List(a), Type::List(b)) => self.unify((a.as_ref(), s1), (b.as_ref(), s2)),
            (Type::Fun(p1, b1), Type::Fun(p2, b2)) => {
                self.unify((p1.as_ref(), s1), (p2.as_ref(), s2))?;
                self.unify((b1.as_ref(), s1), (b2.as_ref(), s2))
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
                (self.generalize(&a).to_string(), s1),
                (self.generalize(&b).to_string(), s2),
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
                env.unify(
                    (&head_ty, *ctx.get_span(next_head)),
                    (&next_head_ty, *ctx.get_span(next_head)),
                )?;
                tail = next_tail;
            }

            Type::List(Box::new(head_ty))
        }
        Node::Expr(Expr::Identifier(name)) => env.instantiate(
            env_vars
                .get(&name)
                .ok_or(Error::UnknownIdentifier(name, *ctx.get_span(expr)))?
                .clone(),
        ),
        Node::Expr(Expr::Condition { cond, then, alt }) => {
            let cond_ty = infer_expr(ctx, env, env_vars, cond)?;

            env.unify(
                (&Type::Bool, *ctx.get_span(cond)),
                (&cond_ty, *ctx.get_span(cond)),
            )?;

            let then_ty = infer_expr(ctx, env, env_vars, then)?;
            let alt_ty = infer_expr(ctx, env, env_vars, alt)?;

            env.unify(
                (&then_ty, *ctx.get_span(then)),
                (&alt_ty, *ctx.get_span(alt)),
            )?;

            alt_ty
        }
        Node::Expr(Expr::Prefix { op, rhs }) => {
            let rhs_ty = infer_expr(ctx, env, env_vars, rhs)?;
            match op {
                PrefixOp::Neg => {
                    env.unify(
                        (&Type::Int, *ctx.get_span(rhs)),
                        (&rhs_ty, *ctx.get_span(rhs)),
                    )?;
                    Type::Int
                }
                PrefixOp::Not => {
                    env.unify(
                        (&Type::Bool, *ctx.get_span(rhs)),
                        (&rhs_ty, *ctx.get_span(rhs)),
                    )?;
                    Type::Bool
                }
            }
        }
        Node::Expr(Expr::Infix { lhs, op, rhs }) => {
            let lhs_ty = infer_expr(ctx, env, env_vars, lhs)?;
            let rhs_ty = infer_expr(ctx, env, env_vars, rhs)?;

            match op {
                InfixOp::Add | InfixOp::Sub | InfixOp::Mul | InfixOp::Div | InfixOp::Mod => {
                    env.unify((&lhs_ty, *ctx.get_span(lhs)), (&rhs_ty, *ctx.get_span(rhs)))?;
                    env.add_constraint(TypeClass::Num, lhs_ty.clone(), *ctx.get_span(lhs));
                    rhs_ty
                }
                InfixOp::Lt | InfixOp::Gt | InfixOp::Lte | InfixOp::Gte => {
                    env.unify((&lhs_ty, *ctx.get_span(lhs)), (&rhs_ty, *ctx.get_span(rhs)))?;
                    env.add_constraint(TypeClass::Ord, lhs_ty, *ctx.get_span(lhs));
                    Type::Bool
                }
                InfixOp::Eq | InfixOp::Neq => {
                    env.unify((&lhs_ty, *ctx.get_span(lhs)), (&rhs_ty, *ctx.get_span(rhs)))?;
                    env.add_constraint(TypeClass::Eq, lhs_ty, *ctx.get_span(lhs));
                    Type::Bool
                }
                InfixOp::And | InfixOp::Or => {
                    env.unify(
                        (&Type::Bool, *ctx.get_span(rhs)),
                        (&lhs_ty, *ctx.get_span(lhs)),
                    )?;
                    env.unify(
                        (&Type::Bool, *ctx.get_span(lhs)),
                        (&rhs_ty, *ctx.get_span(rhs)),
                    )?;
                    Type::Bool
                }
                InfixOp::Cons => {
                    env.unify(
                        (&Type::List(Box::new(lhs_ty)), *ctx.get_span(lhs)),
                        (&rhs_ty, *ctx.get_span(rhs)),
                    )?;
                    rhs_ty
                }
                InfixOp::Append => {
                    env.unify((&lhs_ty, *ctx.get_span(lhs)), (&rhs_ty, *ctx.get_span(rhs)))?;
                    rhs_ty
                }
            }
        }
        Node::Expr(Expr::Lambda { param, body }) => match ctx.get_node(param) {
            Node::Expr(Expr::Identifier(id)) => {
                let param_ty = env.fresh_var();
                let mut new_env_vars = env_vars.clone();
                new_env_vars.insert(id.clone(), param_ty.clone());
                let body_ty = infer_expr(ctx, env, &new_env_vars, body)?;
                Type::Fun(Box::new(param_ty), Box::new(body_ty))
            }
            _ => unreachable!(),
        },
        Node::Expr(Expr::Application { func, arg }) => {
            let func_ty = infer_expr(ctx, env, env_vars, func)?;
            let arg_ty = infer_expr(ctx, env, env_vars, arg)?;
            let ret_ty = env.fresh_var();
            env.unify(
                (&func_ty, *ctx.get_span(func)),
                (
                    &Type::Fun(Box::new(arg_ty), Box::new(ret_ty.clone())),
                    *ctx.get_span(expr),
                ),
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
