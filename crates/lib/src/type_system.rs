use std::{
    collections::VecDeque,
    fmt::{self, Debug, Display, Formatter},
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
    matching,
    scoped::ScopedMap,
    span::Span,
};

#[derive(Debug)]
pub enum Error {
    TypeMismatch(String, String, Span),
    InfiniteType(String, Span),
    UnboundIdentifier(String, Span),
    NonExhaustiveMatch(Vec<String>, Span),
    RedundantMatchArm(String, Span),
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Var(usize),
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

    pub const fn is_var(&self) -> bool {
        matches!(self, Type::Var(..))
    }

    pub const fn is_unit(&self) -> bool {
        matches!(self, Type::Unit)
    }

    pub const fn is_int(&self) -> bool {
        matches!(self, Type::Int)
    }

    pub const fn is_char(&self) -> bool {
        matches!(self, Type::Char)
    }

    pub const fn is_lambda(&self) -> bool {
        matches!(self, Type::Lambda(..))
    }

    pub const fn is_adt(&self) -> bool {
        matches!(self, Type::Adt(..))
    }

    pub fn as_var(&self) -> Option<usize> {
        match self {
            Type::Var(id) => Some(*id),
            _ => None,
        }
    }

    pub fn as_lambda(&self) -> Option<(&Type, &Type)> {
        match self {
            Type::Lambda(lhs, rhs) => Some((lhs, rhs)),
            _ => None,
        }
    }

    pub fn as_adt(&self) -> Option<(&str, &[Type])> {
        match self {
            Type::Adt(name, params) => Some((name, params)),
            _ => None,
        }
    }

    pub fn as_mut_lambda(&mut self) -> Option<(&mut Type, &mut Type)> {
        match self {
            Type::Lambda(lhs, rhs) => Some((lhs.as_mut(), rhs.as_mut())),
            _ => None,
        }
    }

    pub fn as_mut_adt(&mut self) -> Option<(&mut String, &mut Vec<Type>)> {
        match self {
            Type::Adt(name, params) => Some((name, params)),
            _ => None,
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

#[derive(Debug, Clone)]
pub struct TypeScheme {
    pub vars: Vec<usize>,
    pub ty: Type,
}

impl Display for TypeScheme {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "<{}> {}",
            self.vars
                .iter()
                .enumerate()
                .format_with(", ", |(i, _), f| if i < 26 {
                    f(&format!("{}", (97 + i) as u8 as char))
                } else {
                    f(&format!("t{}", i))
                }),
            self.ty
        )
    }
}

#[derive(Debug)]
struct TypeSystem<'a> {
    nodes: &'a mut NodeArena,
    vars: Vec<Option<Type>>,
    rigid_vars: FxHashSet<usize>,
    scheme_scopes: ScopedMap<String, TypeScheme>,
    type_scopes: ScopedMap<String, Type>,
    constraints: VecDeque<Constraint>,
    type_ctors: FxHashMap<String, FxHashMap<String, (usize, usize)>>,
}

impl<'a> TypeSystem<'a> {
    pub fn new(nodes: &'a mut NodeArena) -> Self {
        Self {
            nodes,
            vars: Vec::default(),
            rigid_vars: FxHashSet::default(),
            scheme_scopes: ScopedMap::default(),
            type_scopes: ScopedMap::default(),
            constraints: VecDeque::default(),
            type_ctors: FxHashMap::default(),
        }
    }

    fn fresh_var(&mut self) -> Type {
        let id = self.vars.len();
        let var = Type::Var(id);
        self.vars.push(None);
        var
    }

    fn fresh_rigid_var(&mut self) -> Type {
        let id = self.vars.len();
        let var = Type::Var(id);
        self.vars.push(None);
        self.rigid_vars.insert(id);
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

    fn unify(&mut self, lhs: &Type, rhs: &Type, span: Span) -> Result<()> {
        match (self.resolve(lhs), self.resolve(rhs)) {
            (Type::Int, Type::Int) | (Type::Unit, Type::Unit) | (Type::Char, Type::Char) => Ok(()),
            (Type::Var(a), Type::Var(b)) => {
                if a == b {
                    Ok(())
                } else if self.rigid_vars.contains(&a) {
                    self.bind(b, Type::Var(a), span)
                } else {
                    self.bind(a, Type::Var(b), span)
                }
            }
            (Type::Lambda(param1, body1), Type::Lambda(param2, body2)) => self
                .unify(&param1, &param2, span)
                .and_then(|_| self.unify(&body1, &body2, span)),
            (Type::Adt(name1, params1), Type::Adt(name2, params2))
                if name1 == name2 && params1.len() == params2.len() =>
            {
                params1
                    .into_iter()
                    .zip(params2.into_iter())
                    .map(|(p1, p2)| self.unify(&p1, &p2, span))
                    .collect::<Result<_>>()
            }
            (Type::Var(id), other) | (other, Type::Var(id)) => self.bind(id, other, span),
            (a, b) => Err(Error::TypeMismatch(a.to_string(), b.to_string(), span)),
        }
    }

    fn bind(&mut self, id: usize, ty: Type, span: Span) -> Result<()> {
        if self.rigid_vars.contains(&id) {
            Err(Error::TypeMismatch(
                Type::Var(id).to_string(),
                ty.to_string(),
                span,
            ))
        } else if self.occurs(id, &ty) {
            Err(Error::InfiniteType(ty.to_string(), span))
        } else {
            self.vars[id] = Some(ty);
            Ok(())
        }
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
                    &self.nodes.ty(node).cloned().expect("node should have type"),
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

    fn subsumes(&mut self, lhs: &TypeScheme, rhs: &TypeScheme, span: Span) -> Result<()> {
        let vars_len = self.vars.len();
        let old_rigid = std::mem::take(&mut self.rigid_vars);

        let mapping = lhs
            .vars
            .iter()
            .copied()
            .map(|id| (id, self.fresh_rigid_var()))
            .filter_map(|(id, v)| match v {
                Type::Var(new_id) => Some((id, new_id)),
                _ => None,
            })
            .collect();
        let lhs_ty = lhs.ty.replace_vars(&mapping);

        let mapping = rhs
            .vars
            .iter()
            .copied()
            .map(|id| (id, self.fresh_var()))
            .filter_map(|(id, v)| match v {
                Type::Var(new_id) => Some((id, new_id)),
                _ => None,
            })
            .collect();
        let rhs_ty = rhs.ty.replace_vars(&mapping);

        let res = self.unify(&lhs_ty, &rhs_ty, span);

        self.vars.truncate(vars_len);
        self.rigid_vars = old_rigid;

        res
    }

    fn eval_type_expr(&mut self, scopes: &mut ScopedMap<String, Type>, expr: Node) -> Result<Type> {
        let ty = match self.nodes.kind(expr).as_type_expr().cloned().unwrap() {
            TypeExpr::Unit => Type::Unit,
            TypeExpr::Identifier(id) => scopes
                .get(&id)
                .ok_or_else(|| Error::UnboundIdentifier(id, self.nodes.span(expr)))
                .cloned()?,
            TypeExpr::Constructor(name, args) => match name.as_str() {
                "Int" => Type::Int,
                "Char" => Type::Char,
                _ => Type::Adt(
                    name,
                    args.iter()
                        .map(|arg| self.eval_type_expr(scopes, *arg))
                        .collect::<Result<Vec<_>>>()?,
                ),
            },
            TypeExpr::Lambda(l, r) => Type::Lambda(
                Box::new(self.eval_type_expr(scopes, l)?),
                Box::new(self.eval_type_expr(scopes, r)?),
            ),
            TypeExpr::Forall(params, body) => {
                scopes.push();
                for p in params {
                    scopes.insert(p, self.fresh_var());
                }
                let ty = self.eval_type_expr(scopes, body)?;
                scopes.pop();
                ty
            }
        };
        self.nodes.set_ty(expr, ty.clone());
        Ok(ty)
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

                    let mut ctors = FxHashMap::default();

                    let adt_ty = Type::Adt(ty_name.clone(), param_tys);

                    for (i, (name, args)) in constructors.into_iter().enumerate() {
                        let mut cons_ty = adt_ty.clone();
                        let arg_tys = args
                            .iter()
                            .map(|a| self.eval_type_expr(&mut scopes, *a))
                            .rev()
                            .collect::<Result<Vec<_>>>()?;
                        ctors.insert(name.clone(), (i, arg_tys.len()));
                        for arg_ty in arg_tys {
                            cons_ty = Type::Lambda(Box::new(arg_ty), Box::new(cons_ty));
                        }
                        let scheme = self.generalize(&cons_ty);
                        self.scheme_scopes.insert(name, scheme);
                    }

                    self.type_ctors.insert(ty_name.clone(), ctors);
                    self.nodes.set_ty(*node, adt_ty);
                }
            }
        }

        for module in &modules {
            for &node in module {
                match self.nodes.kind(node).clone() {
                    NodeKind::Bind(name, type_expr, _) => {
                        if name == "main" {
                            self.nodes.set_ty(node, Type::Unit);
                            self.type_scopes.insert(name, Type::Unit);
                        } else if let Some(type_expr) = type_expr {
                            let ty = self.eval_type_expr(&mut ScopedMap::default(), type_expr)?;
                            let scheme = self.generalize(&ty);
                            self.scheme_scopes.insert(name, scheme.clone());
                            self.nodes.set_scheme(node, scheme);
                        } else {
                            let ty = self.fresh_var();
                            self.type_scopes.insert(name, ty.clone());
                            self.nodes.set_ty(node, ty);
                        }
                    }
                    NodeKind::Primitive(name, type_expr, _) => {
                        let ty = self.eval_type_expr(&mut ScopedMap::default(), type_expr)?;
                        let scheme = self.generalize(&ty);
                        self.scheme_scopes.insert(name, scheme.clone());
                        self.nodes.set_ty(node, scheme.ty.clone());
                        self.nodes.set_scheme(node, scheme);
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
                        self.solve_constraints()?;
                        let inferred_scheme = self.generalize(&expr_ty);
                        if let Some(annotated_scheme) = self.nodes.scheme(node).cloned() {
                            self.subsumes(
                                &annotated_scheme,
                                &inferred_scheme,
                                self.nodes.span(node),
                            )?;
                            self.scheme_scopes.insert(name, annotated_scheme.clone());
                            self.nodes.set_ty(node, annotated_scheme.ty.clone());
                            self.nodes.set_scheme(node, annotated_scheme);
                        } else {
                            self.scheme_scopes.insert(name, inferred_scheme.clone());
                            self.nodes.set_ty(node, inferred_scheme.ty.clone());
                            self.nodes.set_scheme(node, inferred_scheme);
                        }
                    }
                    NodeKind::Type(..) | NodeKind::Primitive(..) => (),
                    _ => unreachable!(),
                }
            }
        }

        for node in self.nodes.nodes() {
            if let Some(ty) = self.nodes.ty(node).cloned() {
                let ty = self.resolve(&ty);
                self.nodes.set_ty(node, ty);
            }
        }

        let matches = self.nodes.nodes().into_iter().filter_map(|n| {
            self.nodes
                .kind(n)
                .as_expr()
                .and_then(|e| e.as_match().map(|(&s, a)| (n, s, a)))
        });

        for (node, _, arms) in matches {
            let matrix = arms
                .iter()
                .copied()
                .map(|(p, b)| (Vec::from([p]), b))
                .collect_vec();
            let width = matrix.first().map(|(r, _)| r.len()).unwrap_or(0);
            let missing = matching::missing(self.nodes, &self.type_ctors, &matrix, width);

            if !missing.is_empty() {
                return Err(Error::NonExhaustiveMatch(
                    missing
                        .iter()
                        .map(|r| r.iter().format(", ").to_string())
                        .collect(),
                    self.nodes.span(node),
                ));
            }

            let redundant = matching::redundant(self.nodes, &self.type_ctors, arms);

            if !redundant.is_empty() {
                let (pattern, span) = &redundant[0];
                return Err(Error::RedundantMatchArm(pattern.to_string(), *span));
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
                .scheme_scopes
                .get(&name)
                .cloned()
                .map(|s| self.instantiate(&s))
                .or_else(|| self.type_scopes.get(&name).cloned())
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
                for node in nodes {
                    match self.nodes.kind(node).clone() {
                        NodeKind::Bind(name, type_expr, expr) => {
                            if let Some(type_expr) = type_expr {
                                let ty =
                                    self.eval_type_expr(&mut ScopedMap::default(), type_expr)?;
                                let scheme = self.generalize(&ty);
                                self.scheme_scopes.insert(name.clone(), scheme.clone());
                                self.nodes.set_scheme(node, scheme);
                            } else {
                                let ty = self.fresh_var();
                                self.type_scopes.insert(name.clone(), ty.clone());
                                self.nodes.set_ty(node, ty);
                            }
                            let expr_ty = self.infer_expr(expr)?;
                            self.solve_constraints()?;
                            let inferred_scheme = self.generalize(&expr_ty);
                            if let Some(annotated_scheme) = self.nodes.scheme(node).cloned() {
                                self.subsumes(
                                    &annotated_scheme,
                                    &inferred_scheme,
                                    self.nodes.span(node),
                                )?;
                                self.scheme_scopes.insert(name, annotated_scheme.clone());
                                self.nodes.set_ty(node, annotated_scheme.ty.clone());
                                self.nodes.set_scheme(node, annotated_scheme);
                            } else {
                                self.scheme_scopes.insert(name, inferred_scheme.clone());
                                self.nodes.set_ty(node, inferred_scheme.ty.clone());
                                self.nodes.set_scheme(node, inferred_scheme);
                            }
                        }
                        NodeKind::Expr(_) => {
                            ty = self.infer_expr(node)?;
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
            Pattern::Wildcard => {
                let ty = self.fresh_var();
                self.nodes.set_ty(pattern, ty.clone());
                Ok(())
            }
            Pattern::Identifier(id) => {
                let ty = self.fresh_var();
                self.nodes.set_ty(pattern, ty.clone());
                self.type_scopes.insert(id.clone(), ty);
                Ok(())
            }
            Pattern::Constructor(name, subpats) => {
                let cons_ty = self
                    .scheme_scopes
                    .get(&name)
                    .cloned()
                    .map(|s| self.instantiate(&s))
                    .ok_or_else(|| {
                        Error::UnboundIdentifier(name.clone(), self.nodes.span(pattern))
                    })?;
                let mut arg_tys = Vec::new();
                let mut result_ty = cons_ty;
                while let Type::Lambda(param, body) = result_ty {
                    arg_tys.push(param);
                    result_ty = *body;
                }
                self.nodes.set_ty(pattern, result_ty.clone());
                if arg_tys.len() != subpats.len() {
                    todo!(
                        "error: pattern constructor {} takes {} arguments but got {} at {}",
                        name,
                        arg_tys.len(),
                        subpats.len(),
                        self.nodes.span(pattern).start
                    );
                }
                for (p, a) in subpats.iter().zip(arg_tys) {
                    self.infer_pattern(*p, &a)?;
                }
                Ok(())
            }
            Pattern::Or(alts) => {
                let ty = self.fresh_var();
                self.nodes.set_ty(pattern, ty.clone());
                for alt in alts {
                    self.infer_pattern(alt, expected_ty)?;
                }
                Ok(())
            }
        }
    }
}

pub fn infer(nodes: &mut NodeArena) -> Result<()> {
    TypeSystem::new(nodes).infer()
}
