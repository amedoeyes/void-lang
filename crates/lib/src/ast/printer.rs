use std::fmt::{self, Write};

use itertools::Itertools;

use crate::{
    ast::{
        arena::NodeArena,
        expr::Expr,
        node::{Node, NodeKind},
        pattern::Pattern,
        type_expr::TypeExpr,
    },
    lexer::is_symbol,
};

pub struct Printer<'a> {
    nodes: &'a NodeArena,
}

impl<'a> Printer<'a> {
    pub fn new(nodes: &'a NodeArena) -> Self {
        Self { nodes }
    }

    pub fn fmt(&self, w: &mut impl Write, node: Node, indent: usize) -> fmt::Result {
        match self.nodes.kind(node) {
            NodeKind::Module(nodes) => {
                for n in nodes {
                    self.fmt(w, *n, indent)?;
                    write!(w, "\n\n")?;
                }
                Ok(())
            }
            NodeKind::TypeExpr(expr) => self.fmt_type_expr(w, expr, indent),
            NodeKind::Expr(expr) => self.fmt_expr(w, expr, indent),
            NodeKind::Type(name, params, ctors) => {
                write!(w, "type {name} = ")?;
                if !params.is_empty() {
                    write!(w, "<{}> enum ", params.join(", "))?;
                }
                write!(w, "{{")?;
                if !ctors.is_empty() {
                    writeln!(w)?;
                    for (name, args) in ctors.iter() {
                        write_indent(w, indent + 1)?;
                        write!(w, "{name}")?;
                        if !args.is_empty() {
                            write!(
                                w,
                                " {}",
                                args.iter().copied().map(|a| self.to_string(a)).join(" ")
                            )?;
                        }
                        writeln!(w, ",")?;
                    }
                    write_indent(w, indent)?;
                }
                write!(w, "}};")?;
                Ok(())
            }
            NodeKind::Primitive(name, type_expr, link_name) => {
                write!(w, "primitive {name} : ")?;
                self.fmt(w, *type_expr, indent)?;
                write!(w, " = \"{link_name}\";")?;
                Ok(())
            }
            NodeKind::Bind(name, type_expr, expr) => {
                write!(w, "let ")?;
                let is_symbol = name.chars().nth(0).map(is_symbol).unwrap_or(false);
                if is_symbol {
                    write!(w, "(")?;
                }
                write!(w, "{name}")?;
                if is_symbol {
                    write!(w, ")")?;
                }
                if let Some(type_expr) = type_expr {
                    write!(w, " : ")?;
                    self.fmt(w, *type_expr, indent)?;
                }
                write!(w, " = ")?;
                self.fmt(w, *expr, indent)?;
                write!(w, ";")?;
                Ok(())
            }
            NodeKind::Pattern(pattern) => self.fmt_pattern(w, pattern),
        }
    }

    fn fmt_type_expr(&self, w: &mut impl Write, expr: &TypeExpr, indent: usize) -> fmt::Result {
        match expr {
            TypeExpr::Unit => write!(w, "()"),
            TypeExpr::Identifier(id) => write!(w, "{id}"),
            TypeExpr::Constructor(name, args) => {
                write!(w, "{name}")?;
                if !args.is_empty() {
                    write!(
                        w,
                        "<{}>",
                        args.iter().copied().map(|a| self.to_string(a)).join(" ")
                    )?;
                }
                Ok(())
            }
            TypeExpr::Lambda(l, r) => {
                match self
                    .nodes
                    .kind(*l)
                    .as_type_expr()
                    .expect("node should be type expr")
                {
                    TypeExpr::Lambda(..) => {
                        write!(w, "(")?;
                        self.fmt(w, *l, indent)?;
                        write!(w, ")")?;
                    }
                    _ => self.fmt(w, *l, indent)?,
                }
                write!(w, " -> ")?;
                self.fmt(w, *r, indent)?;
                Ok(())
            }
            TypeExpr::Forall(params, body) => {
                write!(w, "<{}> ", params.iter().join(", "))?;
                self.fmt(w, *body, indent)?;
                Ok(())
            }
        }
    }

    fn fmt_expr(&self, w: &mut impl Write, expr: &Expr, indent: usize) -> fmt::Result {
        match expr {
            Expr::Unit => write!(w, "()"),
            Expr::Char(val) => write!(w, "'{}'", val.escape_default()),
            Expr::Integer(val) => write!(w, "{val}"),
            Expr::Constructor(cons) => write!(w, "{cons}"),
            Expr::Identifier(id) => {
                let is_symbol = id.chars().nth(0).map(is_symbol).unwrap_or(false);
                if is_symbol {
                    write!(w, "(")?;
                }
                write!(w, "{id}")?;
                if is_symbol {
                    write!(w, ")")?;
                }
                Ok(())
            }
            Expr::Match(scrutinee, arms) => {
                write!(w, "match ")?;
                self.fmt(w, *scrutinee, indent)?;
                write!(w, " with {{")?;
                if !arms.is_empty() {
                    writeln!(w)?;
                    for (pat, body) in arms.iter().copied() {
                        write_indent(w, indent + 1)?;
                        self.fmt(w, pat, indent + 1)?;
                        write!(w, " => ")?;
                        self.fmt(w, body, indent + 1)?;
                        writeln!(w, ",")?;
                    }
                    write_indent(w, indent)?;
                }
                write!(w, "}}")?;
                Ok(())
            }
            Expr::Block(nodes) => {
                write!(w, "{{")?;
                if !nodes.is_empty() {
                    writeln!(w)?;
                    for n in nodes.iter().copied() {
                        write_indent(w, indent + 1)?;
                        self.fmt(w, n, indent + 1)?;
                        writeln!(w)?;
                    }
                    write_indent(w, indent)?;
                }
                write!(w, "}}")?;
                Ok(())
            }
            Expr::Lambda(l, r) => {
                write!(w, "{l} -> ")?;
                self.fmt(w, *r, indent)?;
                Ok(())
            }
            Expr::Application(l, r) => {
                if !matches!(
                    self.nodes.kind(*l),
                    NodeKind::Expr(
                        Expr::Unit
                            | Expr::Char(..)
                            | Expr::Integer(..)
                            | Expr::Constructor(..)
                            | Expr::Identifier(..)
                            | Expr::Application(..)
                    ),
                ) {
                    write!(w, "(")?;
                    self.fmt(w, *l, indent)?;
                    write!(w, ")")?;
                } else {
                    self.fmt(w, *l, indent)?;
                }

                write!(w, " ")?;

                if !matches!(
                    self.nodes.kind(*r),
                    NodeKind::Expr(
                        Expr::Unit
                            | Expr::Char(..)
                            | Expr::Integer(..)
                            | Expr::Constructor(..)
                            | Expr::Identifier(..)
                    )
                ) {
                    write!(w, "(")?;
                    self.fmt(w, *r, indent)?;
                    write!(w, ")")?;
                } else {
                    self.fmt(w, *r, indent)?;
                }

                Ok(())
            }
        }
    }

    fn fmt_pattern(&self, w: &mut impl Write, pattern: &Pattern) -> fmt::Result {
        match pattern {
            Pattern::Wildcard => write!(w, "_"),
            Pattern::Identifier(id) => write!(w, "{id}"),
            Pattern::Constructor(name, subpats) => {
                write!(w, "{name}")?;
                if !subpats.is_empty() {
                    write!(
                        w,
                        " {}",
                        subpats.iter().copied().map(|p| self.to_string(p)).join(" ")
                    )?;
                }
                Ok(())
            }
            Pattern::Or(alts) => write!(
                w,
                "{}",
                alts.iter()
                    .copied()
                    .map(|p| self.to_string(p))
                    .format(" | ")
            ),
        }
    }

    pub fn to_string(&self, node: Node) -> String {
        let mut output = String::new();
        self.fmt(&mut output, node, 0).unwrap();
        output
    }
}

pub struct Dumper<'a> {
    nodes: &'a NodeArena,
}

impl<'a> Dumper<'a> {
    pub fn new(nodes: &'a NodeArena) -> Self {
        Self { nodes }
    }

    pub fn fmt(&self, w: &mut impl Write, node: Node, indent: usize) -> fmt::Result {
        let kind = self.nodes.kind(node);
        let span = self.nodes.span(node);

        write_indent(w, indent)?;
        write!(w, "#{} ", node.0)?;

        match kind {
            NodeKind::Module(..) => write!(w, "Module")?,
            NodeKind::TypeExpr(..) => write!(w, "TypeExpr")?,
            NodeKind::Expr(..) => write!(w, "Expr")?,
            NodeKind::Type(..) => write!(w, "Type")?,
            NodeKind::Primitive(..) => write!(w, "Primitive")?,
            NodeKind::Bind(..) => write!(w, "Bind")?,
            NodeKind::Pattern(..) => write!(w, "Pattern")?,
        }

        write!(w, " [span: {}-{}]", span.start, span.end)?;

        if let Some(scheme) = self.nodes.scheme(node) {
            write!(w, " [scheme: {scheme}]")?;
        }

        if let Some(ty) = self.nodes.ty(node) {
            write!(w, " [type: {ty}]")?;
        }

        writeln!(w)?;

        match kind {
            NodeKind::Module(children) => {
                for child in children {
                    self.fmt(w, *child, indent + 1)?;
                }
            }
            NodeKind::TypeExpr(texpr) => self.fmt_type_expr(w, texpr, indent + 1)?,
            NodeKind::Expr(expr) => self.fmt_expr(w, expr, indent + 1)?,
            NodeKind::Type(name, params, ctors) => {
                write_indent(w, indent + 1)?;
                writeln!(w, "name: {name:?}")?;
                if !params.is_empty() {
                    write_indent(w, indent + 1)?;
                    writeln!(w, "params: {params:?}")?;
                }
                for (ctor, args) in ctors {
                    write_indent(w, indent + 1)?;
                    writeln!(w, "constructor:")?;
                    write_indent(w, indent + 2)?;
                    writeln!(w, "name: {ctor:?}")?;
                    if !args.is_empty() {
                        write_indent(w, indent + 2)?;
                        writeln!(w, "args:")?;
                        for arg in args {
                            self.fmt(w, *arg, indent + 3)?;
                        }
                    }
                }
            }
            NodeKind::Primitive(name, type_expr, link_name) => {
                write_indent(w, indent + 1)?;
                writeln!(w, "name: {name:?}")?;
                write_indent(w, indent + 1)?;
                writeln!(w, "type_expr:")?;
                self.fmt(w, *type_expr, indent + 2)?;
                write_indent(w, indent + 1)?;
                writeln!(w, "link_name: {link_name:?}")?;
            }
            NodeKind::Bind(name, type_expr, expr) => {
                write_indent(w, indent + 1)?;
                writeln!(w, "name: {name:?}")?;
                if let Some(ty_expr) = type_expr {
                    write_indent(w, indent + 1)?;
                    writeln!(w, "type_expr:")?;
                    self.fmt(w, *ty_expr, indent + 2)?;
                }
                write_indent(w, indent + 1)?;
                writeln!(w, "expr:")?;
                self.fmt(w, *expr, indent + 2)?;
            }
            NodeKind::Pattern(pattern) => {
                self.fmt_pattern(w, pattern, indent + 1)?;
            }
        }
        Ok(())
    }

    fn fmt_type_expr(&self, w: &mut impl Write, texpr: &TypeExpr, indent: usize) -> fmt::Result {
        match texpr {
            TypeExpr::Unit => {
                write_indent(w, indent)?;
                writeln!(w, "unit")
            }
            TypeExpr::Identifier(id) => {
                write_indent(w, indent)?;
                writeln!(w, "identifier: {id:?}")
            }
            TypeExpr::Constructor(name, args) => {
                write_indent(w, indent)?;
                writeln!(w, "constructor:")?;
                write_indent(w, indent + 1)?;
                writeln!(w, "name: {name:?}")?;
                if !args.is_empty() {
                    write_indent(w, indent + 1)?;
                    writeln!(w, "args:")?;
                    for arg in args {
                        self.fmt(w, *arg, indent + 2)?;
                    }
                }
                Ok(())
            }
            TypeExpr::Lambda(l, r) => {
                write_indent(w, indent)?;
                writeln!(w, "lambda:")?;
                write_indent(w, indent + 1)?;
                writeln!(w, "left:")?;
                self.fmt(w, *l, indent + 2)?;
                write_indent(w, indent + 1)?;
                writeln!(w, "right:")?;
                self.fmt(w, *r, indent + 2)?;
                Ok(())
            }
            TypeExpr::Forall(params, body) => {
                write_indent(w, indent)?;
                writeln!(w, "forall:")?;
                write_indent(w, indent + 1)?;
                writeln!(w, "params: {params:?}")?;
                write_indent(w, indent + 1)?;
                writeln!(w, "body:")?;
                self.fmt(w, *body, indent + 2)?;
                Ok(())
            }
        }
    }

    fn fmt_expr(&self, w: &mut impl Write, expr: &Expr, indent: usize) -> fmt::Result {
        match expr {
            Expr::Unit => {
                write_indent(w, indent)?;
                writeln!(w, "unit")
            }
            Expr::Char(c) => {
                write_indent(w, indent)?;
                writeln!(w, "char: {c:?}")
            }
            Expr::Integer(i) => {
                write_indent(w, indent)?;
                writeln!(w, "integer: {i}")
            }
            Expr::Constructor(name) => {
                write_indent(w, indent)?;
                writeln!(w, "constructor: {name:?}")
            }
            Expr::Identifier(name) => {
                write_indent(w, indent)?;
                writeln!(w, "identifier: {name:?}")
            }
            Expr::Match(scrutinee, arms) => {
                write_indent(w, indent)?;
                writeln!(w, "match:")?;
                write_indent(w, indent + 1)?;
                writeln!(w, "scrutinee:")?;
                self.fmt(w, *scrutinee, indent + 2)?;
                write_indent(w, indent + 1)?;
                writeln!(w, "arms:")?;
                for (pat, body) in arms {
                    write_indent(w, indent + 2)?;
                    writeln!(w, "pattern:")?;
                    self.fmt(w, *pat, indent + 3)?;
                    write_indent(w, indent + 2)?;
                    writeln!(w, "body:")?;
                    self.fmt(w, *body, indent + 3)?;
                }
                Ok(())
            }
            Expr::Block(nodes) => {
                write_indent(w, indent)?;
                writeln!(w, "block:")?;
                for n in nodes {
                    self.fmt(w, *n, indent + 1)?;
                }
                Ok(())
            }
            Expr::Lambda(param, body) => {
                write_indent(w, indent)?;
                writeln!(w, "lambda:")?;
                write_indent(w, indent + 1)?;
                writeln!(w, "param: {param:?}")?;
                write_indent(w, indent + 1)?;
                writeln!(w, "body:")?;
                self.fmt(w, *body, indent + 2)?;
                Ok(())
            }
            Expr::Application(l, r) => {
                write_indent(w, indent)?;
                writeln!(w, "Application:")?;
                write_indent(w, indent + 1)?;
                writeln!(w, "left:")?;
                self.fmt(w, *l, indent + 2)?;
                write_indent(w, indent + 1)?;
                writeln!(w, "right:")?;
                self.fmt(w, *r, indent + 2)?;
                Ok(())
            }
        }
    }

    fn fmt_pattern(&self, w: &mut impl Write, pattern: &Pattern, indent: usize) -> fmt::Result {
        match pattern {
            Pattern::Wildcard => {
                write_indent(w, indent)?;
                writeln!(w, "wildcard")
            }
            Pattern::Identifier(name) => {
                write_indent(w, indent)?;
                writeln!(w, "identifier: {name:?}")
            }
            Pattern::Constructor(name, args) => {
                write_indent(w, indent)?;
                writeln!(w, "constructor:")?;
                write_indent(w, indent + 1)?;
                writeln!(w, "name: {name:?}")?;
                if !args.is_empty() {
                    write_indent(w, indent + 1)?;
                    writeln!(w, "args:")?;
                    for arg in args {
                        self.fmt(w, *arg, indent + 2)?;
                    }
                }
                Ok(())
            }
            Pattern::Or(alts) => {
                write_indent(w, indent)?;
                writeln!(w, "or:")?;
                for alt in alts {
                    self.fmt(w, *alt, indent + 1)?;
                }
                Ok(())
            }
        }
    }

    pub fn to_string(&self, node: Node) -> String {
        let mut output = String::new();
        self.fmt(&mut output, node, 0).unwrap();
        output
    }
}

fn write_indent(w: &mut impl Write, indent: usize) -> fmt::Result {
    Ok(for _ in 0..indent {
        write!(w, "    ")?;
    })
}
