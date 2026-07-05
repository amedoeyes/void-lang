use std::{
    collections::VecDeque,
    fmt::{self, Display, Formatter},
};

use fxhash::{FxHashMap, FxHashSet};
use itertools::Itertools;

use crate::{
    ast::{
        arena::NodeArena,
        expr::Expr,
        node::{Node, NodeKind},
        pattern::Pattern,
        type_expr::TypeExpr,
    },
    scoped::ScopedMap,
    span::Span,
    type_system::Error::TypeMismatch,
};

#[derive(Debug)]
pub enum Error {
    TypeMismatch(String, String, Span),
    InfiniteType(String, Span),
    UnboundIdentifier(String, Span),
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum Type {
    Var(usize),
    #[default]
    Unit,
    Int,
    Char,
    Lambda(Box<Type>, Box<Type>),
    Adt(String, Vec<Type>),
}

impl Type {
    pub fn arity(&self) -> usize {
        match self {
            Type::Lambda(_, r) => 1 + r.arity(),
            _ => 0,
        }
    }

    fn free_vars(&self) -> FxHashSet<usize> {
        let mut vars = FxHashSet::default();
        match &self {
            Type::Var(id) => {
                vars.insert(*id);
            }
            Type::Lambda(l, r) => {
                vars.extend(l.free_vars());
                vars.extend(r.free_vars());
            }
            Type::Adt(_, params) => {
                for p in params {
                    vars.extend(p.free_vars())
                }
            }
            _ => {}
        }
        vars
    }

    fn replace_vars(&self, mapping: &FxHashMap<usize, usize>) -> Type {
        match self {
            Type::Var(id) if let Some(new_id) = mapping.get(&id) => Type::Var(*new_id),
            Type::Lambda(l, r) => Type::Lambda(
                Box::new(l.replace_vars(mapping)),
                Box::new(r.replace_vars(mapping)),
            ),
            Type::Adt(name, params) => Type::Adt(
                name.clone(),
                params.iter().map(|p| p.replace_vars(mapping)).collect(),
            ),
            _ => self.clone(),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        fn fmt_ordered(
            f: &mut Formatter,
            order_map: &FxHashMap<usize, usize>,
            ty: &Type,
        ) -> fmt::Result {
            match ty {
                Type::Unit => write!(f, "()"),
                Type::Int => write!(f, "Int"),
                Type::Char => write!(f, "Char"),
                Type::Var(id) => {
                    let id = *order_map.get(id).expect("var should be mapped");
                    if id < 26 {
                        write!(f, "{}", (97 + id) as u8 as char)
                    } else {
                        write!(f, "t{}", id)
                    }
                }
                Type::Lambda(l, r) => {
                    match l.as_ref() {
                        Type::Lambda(..) => {
                            write!(f, "(")?;
                            fmt_ordered(f, order_map, l)?;
                            write!(f, ")")?;
                        }
                        _ => fmt_ordered(f, order_map, l)?,
                    }
                    write!(f, " -> ")?;
                    fmt_ordered(f, order_map, r)?;
                    Ok(())
                }
                Type::Adt(name, params) => {
                    write!(f, "{}", name)?;
                    if !params.is_empty() {
                        write!(f, "<")?;
                        let mut params = params.iter();
                        fmt_ordered(f, order_map, params.next().unwrap())?;
                        for p in params {
                            write!(f, ", ")?;
                            fmt_ordered(f, order_map, p)?;
                        }
                        write!(f, ">")?;
                    }
                    Ok(())
                }
            }
        }

        fmt_ordered(
            f,
            &self
                .free_vars()
                .iter()
                .sorted()
                .enumerate()
                .map(|(i, t)| (*t, i))
                .collect(),
            self,
        )
    }
}

#[derive(Debug)]
pub enum Constraint {
    Equal(Type, Node),
}

#[derive(Debug, Clone, Default)]
pub struct TypeScheme {
    pub vars: Vec<usize>,
    pub ty: Type,
}

#[derive(Debug)]
struct TypeSystem<'a> {
    nodes: &'a mut NodeArena,
    vars: Vec<Option<Type>>,
    scheme_scopes: ScopedMap<String, TypeScheme>,
    type_scopes: ScopedMap<String, Type>,
    constraints: VecDeque<Constraint>,
}

impl<'a> TypeSystem<'a> {
    pub fn new(nodes: &'a mut NodeArena) -> Self {
        Self {
            nodes,
            vars: Vec::default(),
            scheme_scopes: ScopedMap::default(),
            type_scopes: ScopedMap::default(),
            constraints: VecDeque::default(),
        }
    }

    fn fresh_var(&mut self) -> Type {
        let var = Type::Var(self.vars.len());
        self.vars.push(None);
        var
    }

    fn resolve(&mut self, ty: &Type) -> Type {
        match ty {
            Type::Var(id) if let Some(ty) = self.vars[*id].take() => {
                let ty = self.resolve(&ty);
                self.vars[*id] = Some(ty.clone());
                ty
            }
            Type::Lambda(l, r) => {
                Type::Lambda(Box::new(self.resolve(l)), Box::new(self.resolve(r)))
            }
            Type::Adt(name, params) => Type::Adt(
                name.clone(),
                params.iter().map(|p| self.resolve(p)).collect(),
            ),
            _ => ty.clone(),
        }
    }

    fn unify(&mut self, ty1: &Type, ty2: &Type, span: Span) -> Result<()> {
        match (self.resolve(ty1), self.resolve(ty2)) {
            (Type::Int, Type::Int) | (Type::Unit, Type::Unit) | (Type::Char, Type::Char) => Ok(()),
            (Type::Var(a), Type::Var(b)) if a == b => Ok(()),
            (Type::Lambda(p1, b1), Type::Lambda(p2, b2)) => {
                self.unify(&p1, &p2, span)?;
                self.unify(&b1, &b2, span)?;
                Ok(())
            }
            (Type::Adt(name1, params1), Type::Adt(name2, params2))
                if name1 == name2 && params1.len() == params2.len() =>
            {
                for (arg1, arg2) in params1.into_iter().zip(params2.into_iter()) {
                    self.unify(&arg1, &arg2, span)?;
                }
                Ok(())
            }
            (Type::Var(id), other) | (other, Type::Var(id)) => self.bind(id, other, span),
            (a, b) => Err(Error::TypeMismatch(a.to_string(), b.to_string(), span)),
        }
    }

    fn bind(&mut self, id: usize, ty: Type, span: Span) -> Result<()> {
        if self.occurs(id, &ty) {
            return Err(Error::InfiniteType(ty.to_string(), span));
        }
        self.vars[id] = Some(ty);
        Ok(())
    }

    fn occurs(&self, id: usize, ty: &Type) -> bool {
        match ty {
            Type::Var(other_id) => *other_id == id,
            Type::Lambda(l, r) => self.occurs(id, l) || self.occurs(id, r),
            Type::Adt(_, params) => params.into_iter().any(|p| self.occurs(id, p)),
            _ => false,
        }
    }

    fn solve_constraints(&mut self) -> Result<()> {
        while let Some(cons) = self.constraints.pop_front() {
            match cons {
                Constraint::Equal(ty, node) => self.unify(
                    &ty,
                    &self.nodes.ty(node).unwrap().clone(),
                    self.nodes.span(node),
                )?,
            }
        }
        Ok(())
    }

    fn env_vars(&mut self) -> FxHashSet<usize> {
        self.type_scopes
            .values()
            .cloned()
            .collect::<Vec<_>>()
            .iter()
            .flat_map(|v| self.resolve(v).free_vars())
            .collect()
    }

    fn generalize(&mut self, ty: &Type) -> TypeScheme {
        let ty = self.resolve(ty);
        let vars = ty
            .free_vars()
            .difference(&self.env_vars())
            .copied()
            .collect();
        TypeScheme { vars, ty }
    }

    fn instantiate(&mut self, scheme: &TypeScheme) -> Type {
        let mapping = scheme
            .vars
            .iter()
            .copied()
            .map(|id| (id, self.fresh_var()))
            .filter_map(|(id, v)| match v {
                Type::Var(new_id) => Some((id, new_id)),
                _ => None,
            })
            .collect();

        scheme.ty.replace_vars(&mapping)
    }

    fn eval_type_expr(&mut self, scopes: &mut ScopedMap<String, Type>, expr: Node) -> Result<Type> {
        match self.nodes.kind(expr).as_type_expr().cloned().unwrap() {
            TypeExpr::Unit => Ok(Type::Unit),
            TypeExpr::Identifier(id) => scopes
                .get(&id)
                .ok_or_else(|| Error::UnboundIdentifier(id, self.nodes.span(expr)))
                .cloned(),
            TypeExpr::Constructor(name, args) => match name.as_str() {
                "Int" => Ok(Type::Int),
                "Char" => Ok(Type::Char),
                _ => Ok(Type::Adt(
                    name,
                    args.iter()
                        .map(|arg| self.eval_type_expr(scopes, *arg))
                        .collect::<Result<Vec<_>>>()?,
                )),
            },
            TypeExpr::Lambda(l, r) => Ok(Type::Lambda(
                Box::new(self.eval_type_expr(scopes, l)?),
                Box::new(self.eval_type_expr(scopes, r)?),
            )),
            TypeExpr::Forall(params, body) => {
                scopes.push();
                for p in params {
                    scopes.insert(p, self.fresh_var());
                }
                let ty = self.eval_type_expr(scopes, body);
                scopes.pop();
                ty
            }
        }
    }

    pub fn infer(mut self) -> Result<()> {
        let modules = self
            .nodes
            .kinds()
            .iter()
            .filter_map(|n| match n {
                NodeKind::Module(nodes) => Some(nodes.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();

        for module in &modules {
            for node in module {
                if let NodeKind::Type(ty_name, params, constructors) =
                    self.nodes.kind(*node).clone()
                {
                    let mut param_tys = Vec::with_capacity(params.len());
                    let mut scopes = ScopedMap::default();
                    for param in params {
                        let ty = self.fresh_var();
                        param_tys.push(ty.clone());
                        scopes.insert(param, ty);
                    }

                    let adt_ty = Type::Adt(ty_name, param_tys);

                    for (name, args) in constructors {
                        let mut cons_ty = adt_ty.clone();
                        for arg_ty in args
                            .iter()
                            .map(|a| self.eval_type_expr(&mut scopes, *a))
                            .rev()
                        {
                            cons_ty = Type::Lambda(Box::new(arg_ty?), Box::new(cons_ty));
                        }
                        let scheme = self.generalize(&cons_ty);
                        self.scheme_scopes.insert(name, scheme);
                    }

                    self.nodes.set_ty(*node, adt_ty);
                }
            }
        }

        for module in &modules {
            for node in module {
                match self.nodes.kind(*node).clone() {
                    NodeKind::Bind(name, type_expr, ..) => {
                        let ty = if name == "main" {
                            Type::Unit
                        } else {
                            type_expr
                                .map(|t| self.eval_type_expr(&mut ScopedMap::default(), t))
                                .unwrap_or_else(|| Ok(self.fresh_var()))?
                        };
                        self.type_scopes.insert(name, ty.clone());
                        self.nodes.set_ty(*node, ty);
                    }
                    NodeKind::Primitive(name, type_expr, ..) => {
                        let ty = self.eval_type_expr(&mut ScopedMap::default(), type_expr)?;
                        let scheme = self.generalize(&ty);
                        self.scheme_scopes.insert(name, scheme);
                        self.nodes.set_ty(type_expr, ty.clone());
                        self.nodes.set_ty(*node, ty);
                    }
                    _ => (),
                }
            }
        }

        for module in modules {
            for node in module {
                match self.nodes.kind(node).clone() {
                    NodeKind::Bind(name, _, expr) => {
                        let expr_ty = self.infer_expr(expr)?;
                        let ty = self.nodes.ty(node).expect("should have type");
                        self.constraints
                            .push_back(Constraint::Equal(ty.clone(), expr));
                        let scheme = self.generalize(&expr_ty);
                        self.scheme_scopes.insert(name, scheme);
                    }
                    NodeKind::Type(..) | NodeKind::Primitive(..) => continue,
                    _ => unreachable!(),
                }
            }
        }

        self.solve_constraints()?;

        for node in self.nodes.nodes() {
            if let Some(ty) = self.nodes.ty(node).cloned() {
                let ty = self.resolve(&ty);
                self.nodes.set_ty(node, ty);
            }
        }

        Ok(())
    }

    fn infer_expr(&mut self, expr: Node) -> Result<Type> {
        let ty = match self.nodes.kind(expr).as_expr().cloned().unwrap() {
            Expr::Unit => Type::Unit,
            Expr::Char(..) => Type::Char,
            Expr::Integer(..) => Type::Int,
            Expr::Constructor(name) => self
                .scheme_scopes
                .get(&name)
                .cloned()
                .map(|s| self.instantiate(&s))
                .ok_or_else(|| Error::UnboundIdentifier(name, self.nodes.span(expr)))?,
            Expr::Identifier(name) => self
                .type_scopes
                .get(&name)
                .cloned()
                .or_else(|| {
                    self.scheme_scopes
                        .get(&name)
                        .cloned()
                        .map(|s| self.instantiate(&s))
                })
                .ok_or_else(|| Error::UnboundIdentifier(name, self.nodes.span(expr)))?,
            Expr::Match(scrutinee, arms) => {
                let scrutinee_ty = self.infer_expr(scrutinee)?;
                let match_ty = self.fresh_var();
                for (pattern, body) in arms {
                    self.type_scopes.push();
                    self.infer_pattern(pattern, &scrutinee_ty)?;
                    self.infer_expr(body)?;
                    self.constraints
                        .push_back(Constraint::Equal(match_ty.clone(), body));
                    self.type_scopes.pop();
                }
                match_ty
            }
            Expr::Block(nodes) => {
                self.scheme_scopes.push();
                self.type_scopes.push();
                let mut ty = Type::Unit;
                for n in nodes {
                    match self.nodes.kind(n).clone() {
                        NodeKind::Bind(name, type_expr, expr) => {
                            let ty = type_expr
                                .map(|t| self.eval_type_expr(&mut ScopedMap::default(), t))
                                .unwrap_or_else(|| Ok(self.fresh_var()))?;
                            self.nodes.set_ty(n, ty.clone());
                            self.type_scopes.insert(name.clone(), ty.clone());
                            let expr_ty = self.infer_expr(expr)?;
                            let scheme = self.generalize(&expr_ty);
                            self.scheme_scopes.insert(name, scheme);
                            self.constraints.push_back(Constraint::Equal(ty, expr));
                        }
                        NodeKind::Expr(..) => {
                            ty = self.infer_expr(n)?;
                        }
                        _ => unreachable!(),
                    }
                }
                self.scheme_scopes.pop();
                self.type_scopes.pop();
                ty
            }
            Expr::Lambda(l, r) => {
                let l_ty = self.fresh_var();
                self.type_scopes.push();
                self.type_scopes.insert(l, l_ty.clone());
                let r_ty = self.infer_expr(r)?;
                self.type_scopes.pop();
                Type::Lambda(Box::new(l_ty), Box::new(r_ty))
            }
            Expr::Application(l, r) => {
                self.infer_expr(l)?;
                let r_ty = self.infer_expr(r)?;
                let ret_ty = self.fresh_var();
                self.constraints.push_back(Constraint::Equal(
                    Type::Lambda(Box::new(r_ty), Box::new(ret_ty.clone())),
                    l,
                ));
                ret_ty
            }
        };
        self.nodes.set_ty(expr, ty.clone());
        Ok(ty)
    }

    fn infer_pattern(&mut self, pattern: Node, expected_ty: &Type) -> Result<()> {
        self.constraints
            .push_back(Constraint::Equal(expected_ty.clone(), pattern));
        match self
            .nodes
            .kind(pattern)
            .as_pattern()
            .cloned()
            .expect("node should be pattern")
        {
            Pattern::Wildcard => Ok(()),
            Pattern::Identifier(id) => {
                let ty = self.fresh_var();
                self.nodes.set_ty(pattern, ty.clone());
                self.type_scopes.insert(id.clone(), ty);
                Ok(())
            }
            Pattern::Constructor(name, subpatterns) => {
                let cons_ty = self
                    .scheme_scopes
                    .get(&name)
                    .cloned()
                    .map(|s| self.instantiate(&s))
                    .ok_or_else(|| Error::UnboundIdentifier(name, self.nodes.span(pattern)))?;
                let mut arg_tys = Vec::new();
                let mut result_ty = cons_ty;
                while let Type::Lambda(param, body) = result_ty {
                    arg_tys.push(param);
                    result_ty = *body;
                }
                self.nodes.set_ty(pattern, result_ty.clone());
                if arg_tys.len() != subpatterns.len() {
                    todo!(
                        "error: pattern constructor takes {} arguments but got {}",
                        arg_tys.len(),
                        subpatterns.len()
                    );
                }
                for (p, a) in subpatterns.iter().zip(arg_tys) {
                    self.infer_pattern(*p, &a)?;
                }
                Ok(())
            }
        }
    }
}

pub fn infer(nodes: &mut NodeArena) -> Result<()> {
    TypeSystem::new(nodes).infer()
}
