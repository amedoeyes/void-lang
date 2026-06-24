use core::fmt;
use std::collections::BTreeSet;
use std::hash::Hash;
use std::result;

use fxhash::FxHashMap;
use itertools::Itertools;

use crate::context::{Context, Node, NodeId};
use crate::expr::{Pattern, TypeExpr};
use crate::{expr::Expr, span::Span};

#[macro_export]
macro_rules! ty {
    (Int) => { Type::Int };
    (Char) => { Type::Char };
    (()) => { Type::Unit };

    ($param:tt -> $body:tt) => { Type::Fun(Box::new(ty!($param)), Box::new(ty!($body))) };

    ($param:tt -> $body:tt -> $($rest:tt)*) => { Type::Fun(Box::new(ty!($param)), Box::new(ty!($body -> $($rest)*))) };

    ($var:literal) => { Type::Var($var) };

    (($($inner:tt)*)) => { ty!($($inner)*) };

    (forall $($vars:literal)* . ($($class:tt $var:literal),*) => $($body:tt)*) => {
        Type::Poly(
            [$($vars),*].into_iter().collect::<BTreeSet<usize>>(),
            Vec::from([
                $(Constraint {
                    class: TypeClass::$class,
                    ty: Type::Var($var),
                    span: Span::default()
                }),*
            ]),
            Box::new(ty!($($body)*))
        )
    };

    (forall $($vars:literal)* . $($body:tt)*) => {
        Type::Poly(
            [$($vars),*].into_iter().collect::<BTreeSet<usize>>(),
            Vec::new(),
            Box::new(ty!($($body)*))
        )
    };

    ($type:tt $($args:tt)*) => { Type::Adt(stringify!($type).into(), Vec::from([$(ty!($args)),*])) };
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
    Var(usize),
    Fun(Box<Type>, Box<Type>),
    Poly(BTreeSet<usize>, Vec<Constraint>, Box<Type>),
    Adt(String, Vec<Type>),
}

impl Type {
    fn type_vars(&self) -> BTreeSet<usize> {
        let mut vars = BTreeSet::default();
        self.collect_type_vars(&mut vars);
        vars
    }

    fn collect_type_vars(&self, vars: &mut BTreeSet<usize>) {
        match self {
            Type::Var(n) => {
                vars.insert(*n);
            }
            Type::Fun(param, body) => {
                param.collect_type_vars(vars);
                body.collect_type_vars(vars);
            }
            Type::Poly(_, _, body) => body.collect_type_vars(vars),
            Type::Adt(_, params) => {
                for param in params {
                    param.collect_type_vars(vars);
                }
            }
            _ => {}
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            Type::Fun(_, rhs) => 1 + rhs.arity(),
            Type::Poly(_, _, fun) => fun.arity(),
            _ => 0,
        }
    }

    fn fmt(&self, f: &mut fmt::Formatter, var_order_map: &FxHashMap<usize, usize>) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "Unit"),
            Type::Int => write!(f, "Int"),
            Type::Char => write!(f, "Char"),
            Type::Var(n) => write!(f, "t{}", var_order_map.get(n).unwrap_or(n)),
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
            Type::Adt(name, params) => {
                write!(f, "{}", name)?;
                for param in params {
                    write!(f, " ")?;
                    match param {
                        Type::Fun(_, _) | Type::Adt(_, _) => {
                            write!(f, "(")?;
                            param.fmt(f, var_order_map)?;
                            write!(f, ")")?;
                        }
                        _ => param.fmt(f, var_order_map)?,
                    }
                }
                Ok(())
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
            Vec::from([ty!(Int), ty!(Bool), ty!(Char), ty!(())]),
        );
        instances.insert(TypeClass::Ord, Vec::from([ty!(Int), ty!(Bool), ty!(Char)]));
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
            Type::Fun(param, body) => Type::Fun(
                Box::new(self.instantiate_helper(*param, span, substitutions)),
                Box::new(self.instantiate_helper(*body, span, substitutions)),
            ),
            Type::Adt(name, params) => {
                let new_params = params
                    .into_iter()
                    .map(|p| self.instantiate_helper(p, span, substitutions))
                    .collect();
                Type::Adt(name, new_params)
            }
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
            Type::Fun(param, body) => Type::Fun(
                Box::new(self.substitute(param)),
                Box::new(self.substitute(body)),
            ),
            Type::Adt(name, params) => {
                let new_params = params.iter().map(|p| self.substitute(p)).collect();
                Type::Adt(name.clone(), new_params)
            }
            _ => ty.clone(),
        }
    }

    fn unify(&mut self, ty1: &Type, ty2: &Type, span: Span) -> Result<()> {
        match (self.substitute(ty1), self.substitute(ty2)) {
            (Type::Int, Type::Int) | (Type::Unit, Type::Unit) | (Type::Char, Type::Char) => Ok(()),
            (Type::Var(a), Type::Var(b)) if a == b => Ok(()),
            (Type::Fun(p1, b1), Type::Fun(p2, b2)) => {
                self.unify(p1.as_ref(), p2.as_ref(), span)?;
                self.unify(b1.as_ref(), b2.as_ref(), span)
            }
            (Type::Poly(vars1, _, body1), Type::Poly(vars2, _, body2))
                if vars1.len() == vars2.len() =>
            {
                self.unify(&body1, &body2, span)
            }
            (a @ Type::Fun(..), Type::Poly(_, _, b)) => {
                let b = self.instantiate(*b, span);
                self.unify(&a, &b, span)
            }
            (Type::Poly(_, _, a), b @ Type::Fun(..)) => {
                let a = self.instantiate(*a, span);
                self.unify(&a, &b, span)
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
            (Type::Adt(name1, args1), Type::Adt(name2, args2)) if name1 == name2 => {
                if args1.len() != args2.len() {
                    todo!();
                }
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    self.unify(arg1, arg2, span)?;
                }
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
            Type::Fun(param, body) => {
                self.occurs_check(var, param, span)?;
                self.occurs_check(var, body, span)
            }
            Type::Poly(vars, _, body) if !vars.contains(&var) => self.occurs_check(var, body, span),
            _ => Ok(()),
        }
    }
}

fn infer_pattern(
    ctx: &mut Context,
    env: &mut Env,
    env_vars: &FxHashMap<String, Type>,
    pattern: &Pattern,
    expected_ty: &Type,
    span: Span,
) -> Result<FxHashMap<String, Type>> {
    match pattern {
        Pattern::Wildcard => Ok(FxHashMap::default()),
        Pattern::Identifier(id) => {
            let mut map = FxHashMap::default();
            let ty = env.fresh_var();
            env.unify(expected_ty, &ty, span)?;
            map.insert(id.clone(), ty);
            Ok(map)
        }
        Pattern::Constructor(name, subpatterns) => {
            let mut map = FxHashMap::default();
            let cons_ty = env.instantiate(
                env_vars
                    .get(name)
                    .ok_or_else(|| Error::UnknownIdentifier(name.clone(), span))?
                    .clone(),
                span,
            );
            let mut arg_tys = Vec::new();
            let mut result_ty = cons_ty;
            while let Type::Fun(param, body) = result_ty {
                arg_tys.push(param);
                result_ty = *body;
            }
            if arg_tys.len() != subpatterns.len() {
                todo!();
            }
            env.unify(expected_ty, &result_ty, span)?;
            for (p, a) in subpatterns.iter().zip(arg_tys) {
                map.extend(infer_pattern(ctx, env, env_vars, p, &a, span)?);
            }
            Ok(map)
        }
    }
}

fn infer_expr(
    ctx: &mut Context,
    env: &mut Env,
    env_vars: &FxHashMap<String, Type>,
    expr: NodeId,
) -> Result<Type> {
    let ty = ctx.get_type(expr).as_ref();

    let ty = if let Some(ty) = ty {
        ty.clone()
    } else {
        match ctx.get_node(expr).clone() {
            Node::Expr(Expr::Unit) => Type::Unit,
            Node::Expr(Expr::Integer(_)) => Type::Int,
            Node::Expr(Expr::Char(_)) => Type::Char,
            Node::Expr(Expr::Constructor(name)) | Node::Expr(Expr::Identifier(name)) => env
                .instantiate(
                    env_vars
                        .get(&name)
                        .ok_or(Error::UnknownIdentifier(name, ctx.get_span(expr)))?
                        .clone(),
                    ctx.get_span(expr),
                ),
            Node::Expr(Expr::Match(scrutinee, branches)) => {
                let scrutinee_ty = infer_expr(ctx, env, env_vars, scrutinee)?;
                let match_ty = env.fresh_var();
                for (p, b) in branches {
                    let mut branch_env = env_vars.clone();
                    let bindings =
                        infer_pattern(ctx, env, env_vars, &p, &scrutinee_ty, ctx.get_span(expr))?;
                    for (id, ty) in bindings {
                        branch_env.insert(id, ty);
                    }
                    let body_ty = infer_expr(ctx, env, &branch_env, b)?;
                    env.unify(&match_ty, &body_ty, ctx.get_span(expr))?;
                }
                match_ty
            }
            Node::Expr(Expr::Block(nodes)) => {
                let mut new_env_vars = env_vars.clone();
                let mut ty = Type::Unit;
                for n in nodes {
                    match ctx.get_node(n).clone() {
                        Node::Bind(name, type_expr, expr) => {
                            let ty = match type_expr {
                                Some(t) => {
                                    let ty =
                                        eval_type_expr(ctx, env, &mut FxHashMap::default(), t)?;
                                    env.generalize(&ty)
                                }
                                None => env.fresh_var(),
                            };
                            new_env_vars.insert(name.clone(), ty.clone());
                            let expr_ty = infer_expr(ctx, env, &new_env_vars, expr)?;
                            let expr_ty = env.generalize(&env.substitute(&expr_ty));
                            env.unify(&ty, &expr_ty, ctx.get_span(expr))?;
                            new_env_vars.insert(name, ty.clone());
                            ctx.set_type(expr, expr_ty.clone());
                            ctx.set_type(n, ty);
                        }
                        Node::Expr(..) => {
                            ty = infer_expr(ctx, env, &new_env_vars, n)?;
                        }
                        _ => unreachable!(),
                    }
                }
                ty
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
        }
    };

    let ty = env.substitute(&ty);
    ctx.set_type(expr, ty.clone());
    Ok(ty)
}

fn eval_type_expr(
    ctx: &Context,
    env: &mut Env,
    type_vars: &mut FxHashMap<String, Type>,
    expr: NodeId,
) -> Result<Type> {
    match ctx.get_type_expr(expr).unwrap() {
        TypeExpr::Unit => Ok(Type::Unit),
        TypeExpr::Identifier(id) => type_vars
            .get(id)
            .ok_or_else(|| Error::UnknownIdentifier(id.clone(), ctx.get_span(expr)))
            .cloned(),
        TypeExpr::Constructor(name, args) => match name.as_str() {
            "Int" => Ok(Type::Int),
            "Char" => Ok(Type::Char),
            _ => Ok(Type::Adt(
                name.clone(),
                args.iter()
                    .map(|arg| eval_type_expr(ctx, env, type_vars, *arg))
                    .collect::<Result<Vec<_>>>()?,
            )),
        },
        TypeExpr::Lambda(l, r) => Ok(Type::Fun(
            Box::new(eval_type_expr(ctx, env, type_vars, *l)?),
            Box::new(eval_type_expr(ctx, env, type_vars, *r)?),
        )),
        TypeExpr::Forall(params, body) => {
            for p in params {
                type_vars.insert(p.clone(), env.fresh_var());
            }
            eval_type_expr(ctx, env, type_vars, *body)
        }
    }
}

pub fn infer(ctx: &mut Context) -> Result<()> {
    let mut env = Env::new();
    let mut env_vars = FxHashMap::default();

    let modules = ctx
        .nodes()
        .iter()
        .filter_map(|n| match n {
            Node::Module(nodes) => Some(nodes.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();

    for module in &modules {
        for node in module {
            if let Node::Type(ty_name, params, constructors) = ctx.get_node(*node) {
                let mut param_tys = Vec::with_capacity(params.len());
                let mut type_vars = FxHashMap::default();
                for param in params {
                    let ty = env.fresh_var();
                    param_tys.push(ty.clone());
                    type_vars.insert(param.clone(), ty);
                }

                let adt_ty = Type::Adt(ty_name.clone(), param_tys);

                for (cons, args) in constructors {
                    let mut cons_ty = adt_ty.clone();
                    for arg_ty in args
                        .iter()
                        .map(|a| eval_type_expr(ctx, &mut env, &mut type_vars, *a))
                        .rev()
                    {
                        cons_ty = Type::Fun(Box::new(arg_ty?), Box::new(cons_ty));
                    }
                    cons_ty = env.generalize(&cons_ty);
                    env_vars.insert(cons.clone(), cons_ty);
                }

                ctx.set_type(*node, adt_ty);
            }
        }
    }

    for module in &modules {
        for node in module {
            match ctx.get_node(*node) {
                Node::Bind(name, type_expr, ..) => {
                    let ty = match type_expr {
                        Some(t) => {
                            let ty = eval_type_expr(ctx, &mut env, &mut FxHashMap::default(), *t)?;
                            env.generalize(&ty)
                        }
                        None => env.fresh_var(),
                    };
                    env_vars.insert(name.clone(), ty.clone());
                    ctx.set_type(*node, ty);
                }
                Node::Primitive(name, type_expr, ..) => {
                    let ty = eval_type_expr(ctx, &mut env, &mut FxHashMap::default(), *type_expr)?;
                    env_vars.insert(name.clone(), ty.clone());
                    ctx.set_type(*type_expr, ty.clone());
                    ctx.set_type(*node, ty);
                }
                _ => (),
            }
        }
    }

    for module in modules {
        for node in module {
            match ctx.get_node(node).clone() {
                Node::Bind(_, _, expr) => {
                    let expr_ty = infer_expr(ctx, &mut env, &env_vars, expr)?;
                    let expr_ty = env.generalize(&env.substitute(&expr_ty));
                    ctx.set_type(expr, expr_ty.clone());
                    let ty = ctx.get_type(node).as_ref().expect("should have type");
                    env.unify(&ty, &expr_ty, ctx.get_span(expr))?;
                }
                Node::Type(..) | Node::Primitive(..) | Node::Import(..) => continue,
                _ => unreachable!(),
            }
        }
    }

    for node in ctx
        .nodes()
        .iter()
        .enumerate()
        .map(|(i, _)| NodeId(i))
        .collect::<Vec<NodeId>>()
    {
        if let Some(ty) = ctx.get_type(node) {
            ctx.set_type(node, env.substitute(ty));
        }
    }

    env.solve_constraints()?;
    Ok(())
}
