use std::fmt::{self, Display, Formatter};

use fxhash::FxHashMap;
use itertools::Itertools;

use crate::{
    ast::{
        arena::NodeArena,
        expr::Expr,
        node::{Node, NodeKind},
        pattern::Pattern,
    },
    r#match::Match,
};

#[derive(Debug, Clone)]
pub enum Instruction {
    PushInt(i64),
    Alloc,
    Push(usize),
    PushGlobal(String, usize),
    Pop(usize),
    Update(usize),
    Slide(usize),
    MkAp,
    Pack(usize, usize),
    Unpack(usize),
    Case(FxHashMap<usize, Vec<Instruction>>, Option<Vec<Instruction>>),
    Eval,
    Unwind,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        fn write_indent(f: &mut Formatter, indent: usize) -> fmt::Result {
            Ok(for _ in 0..indent {
                write!(f, "    ")?;
            })
        }

        fn fmt(inst: &Instruction, f: &mut Formatter, depth: usize) -> fmt::Result {
            write_indent(f, depth)?;
            match inst {
                Instruction::PushInt(i) => write!(f, "PUSHINT {i}"),
                Instruction::Alloc => write!(f, "ALLOC"),
                Instruction::Push(n) => write!(f, "PUSH {n}"),
                Instruction::PushGlobal(name, arity) => write!(f, "PUSHGLOBAL {name}, {arity}"),
                Instruction::Pop(n) => write!(f, "POP {n}"),
                Instruction::Update(n) => write!(f, "UPDATE {n}"),
                Instruction::Slide(n) => write!(f, "SLIDE {n}"),
                Instruction::MkAp => write!(f, "MKAP"),
                Instruction::Pack(t, a) => write!(f, "PACK {t} {a}"),
                Instruction::Unpack(n) => write!(f, "UNPACK {n}"),
                Instruction::Case(arms, default) => {
                    writeln!(f, "CASE {{")?;
                    for (pattern, body) in arms {
                        write_indent(f, depth + 1)?;
                        writeln!(f, "{pattern} => {{")?;
                        for inst in body {
                            fmt(inst, f, depth + 2)?;
                            writeln!(f)?;
                        }
                        write_indent(f, depth + 1)?;
                        writeln!(f, "}}")?;
                    }
                    if let Some(default) = default {
                        write_indent(f, depth + 1)?;
                        writeln!(f, "_ => {{")?;
                        for inst in default {
                            fmt(inst, f, depth + 2)?;
                            writeln!(f)?;
                        }
                        write_indent(f, depth + 1)?;
                        writeln!(f, "}}")?;
                    }
                    write_indent(f, depth)?;
                    write!(f, "}}")?;
                    Ok(())
                }
                Instruction::Eval => write!(f, "EVAL"),
                Instruction::Unwind => write!(f, "UNWIND"),
            }
        }

        fmt(self, f, 0)
    }
}

#[derive(Debug)]
pub struct IRGenerator<'a> {
    pub nodes: &'a NodeArena,
    pub symbols: FxHashMap<String, Vec<Instruction>>,
    pub symbols_arity: FxHashMap<String, usize>,
    pub symbols_alias: FxHashMap<String, String>,
    pub type_ctors: FxHashMap<String, FxHashMap<String, (usize, usize)>>,
    pub lambda_counter: usize,
}

impl<'a> IRGenerator<'a> {
    pub fn new(nodes: &'a NodeArena) -> Self {
        let type_ctors = nodes
            .kinds()
            .iter()
            .filter_map(|n| match n {
                NodeKind::Type(name, _, constructors) => Some((
                    name.clone(),
                    constructors
                        .iter()
                        .enumerate()
                        .map(|(i, (c, a))| (c.clone(), (i, a.len())))
                        .collect::<FxHashMap<_, _>>(),
                )),
                _ => None,
            })
            .collect::<FxHashMap<_, _>>();

        Self {
            nodes,
            symbols: FxHashMap::default(),
            symbols_arity: FxHashMap::default(),
            symbols_alias: FxHashMap::default(),
            type_ctors,
            lambda_counter: 0,
        }
    }

    pub fn generate(&mut self) {
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
                match self.nodes.kind(*node) {
                    NodeKind::Type(_, _, constructors) => {
                        for (cons, args) in constructors {
                            self.symbols_arity.insert(cons.clone(), args.len());
                        }
                    }
                    NodeKind::Primitive(name, _, link_name) => {
                        if let Some(ty) = self.nodes.ty(*node) {
                            self.symbols_alias.insert(name.clone(), link_name.clone());
                            self.symbols_arity.insert(name.clone(), ty.arity());
                            self.symbols_arity.insert(link_name.clone(), ty.arity());
                        }
                    }
                    NodeKind::Bind(name, ..) => {
                        if let Some(ty) = self.nodes.ty(*node) {
                            self.symbols_arity.insert(name.clone(), ty.arity());
                        }
                    }
                    _ => continue,
                }
            }
        }

        for module in modules {
            for node in module {
                match self.nodes.kind(node) {
                    NodeKind::Type(_, _, constructors) => {
                        for (i, (cons, args)) in constructors.iter().enumerate() {
                            let mut insts = Vec::new();
                            let arity = args.len();
                            insts.push(Instruction::Pack(i, arity));
                            insts.push(Instruction::Update(0));
                            insts.push(Instruction::Unwind);
                            self.symbols.insert(cons.clone(), insts);
                        }
                    }
                    NodeKind::Bind(name, .., expr) => {
                        if let Some(ty) = self.nodes.ty(*expr) {
                            let arity = ty.arity();
                            let mut insts = Vec::new();
                            match self.nodes.kind(*expr) {
                                NodeKind::Expr(Expr::Lambda(..)) => {
                                    let mut offsets = FxHashMap::default();
                                    let mut node = *expr;
                                    while let NodeKind::Expr(Expr::Lambda(l, r)) =
                                        self.nodes.kind(node)
                                    {
                                        offsets.insert(l.clone(), offsets.len());
                                        node = *r;
                                    }
                                    self.generate_expr(node, &offsets, &mut insts)
                                }
                                _ => {
                                    if arity > 0 {
                                        insts.extend(
                                            (0..arity).map(|_| Instruction::Push(arity - 1)),
                                        );
                                    }
                                    self.generate_expr(*expr, &FxHashMap::default(), &mut insts);
                                    if arity > 0 {
                                        insts.extend((0..arity).map(|_| Instruction::MkAp));
                                    }
                                }
                            }
                            insts.push(Instruction::Update(arity));
                            if arity > 0 {
                                insts.push(Instruction::Pop(arity));
                            }
                            insts.push(Instruction::Unwind);
                            self.symbols.insert(name.clone(), insts);
                        }
                    }
                    _ => continue,
                }
            }
        }
    }

    fn collect_free_vars(&self, node: Node, bound: &mut Vec<String>, out: &mut Vec<Node>) {
        match self.nodes.kind(node) {
            NodeKind::Expr(Expr::Identifier(id)) => {
                if !bound.contains(id) {
                    out.push(node);
                }
            }
            NodeKind::Expr(Expr::Application(r, l)) => {
                self.collect_free_vars(*r, bound, out);
                self.collect_free_vars(*l, bound, out);
            }
            NodeKind::Expr(Expr::Match(scrutinee, branches)) => {
                self.collect_free_vars(*scrutinee, bound, out);
                for (pat, body) in branches {
                    let original_len = bound.len();
                    self.collect_pattern_bound_vars(*pat, bound);
                    self.collect_free_vars(*body, bound, out);
                    bound.truncate(original_len);
                }
            }
            _ => {}
        }
    }

    pub fn collect_pattern_bound_vars(&self, pattern: Node, vars: &mut Vec<String>) {
        match self
            .nodes
            .kind(pattern)
            .as_pattern()
            .expect("node should be pattern")
        {
            Pattern::Wildcard => {}
            Pattern::Identifier(id) => vars.push(id.clone()),
            Pattern::Constructor(_, subpats) => {
                for pat in subpats {
                    self.collect_pattern_bound_vars(*pat, vars);
                }
            }
            Pattern::Or(alts) => {
                for alt in alts {
                    self.collect_pattern_bound_vars(*alt, vars);
                }
            }
        }
    }

    fn create_lambda_name(&mut self) -> String {
        let name = format!("__lambda_{}", self.lambda_counter);
        self.lambda_counter += 1;
        name
    }

    pub fn generate_expr(
        &mut self,
        node: Node,
        offsets: &FxHashMap<String, usize>,
        out: &mut Vec<Instruction>,
    ) {
        match self.nodes.kind(node) {
            NodeKind::Expr(expr) => match expr {
                Expr::Unit => out.push(Instruction::Pack(0, 0)),
                Expr::Integer(i) => out.push(Instruction::PushInt(*i)),
                Expr::Char(c) => out.push(Instruction::PushInt(*c as i64)),
                Expr::Constructor(cons) => out.push(Instruction::PushGlobal(
                    cons.clone(),
                    *self.symbols_arity.get(cons).unwrap(),
                )),
                Expr::Identifier(id) => {
                    let id = self.symbols_alias.get(id).unwrap_or(id);
                    if let Some(offset) = offsets.get(id) {
                        out.push(Instruction::Push(*offset));
                    } else {
                        out.push(Instruction::PushGlobal(
                            id.clone(),
                            *self.symbols_arity.get(id).unwrap(),
                        ));
                    }
                }
                Expr::Application(l, r) => {
                    self.generate_expr(*r, offsets, out);
                    self.generate_expr(
                        *l,
                        &offsets.iter().map(|(k, v)| (k.clone(), v + 1)).collect(),
                        out,
                    );
                    out.push(Instruction::MkAp);
                }
                Expr::Match(scrutinee, arms) => {
                    let type_ctors = self.type_ctors.clone(); // for now
                    let r#match = Match::new(
                        self.nodes,
                        &type_ctors,
                        *scrutinee,
                        arms.iter()
                            .copied()
                            .map(|(p, b)| (Vec::from([p]), b))
                            .collect_vec(),
                    );
                    let insts = self.generate_match(r#match, offsets.clone(), Vec::new());
                    out.extend(insts);
                }
                Expr::Block(nodes) => {
                    let mut new_offsets = offsets.clone();
                    let mut binds = 0;
                    for n in nodes {
                        match self.nodes.kind(*n) {
                            NodeKind::Bind(name, .., expr) => {
                                match self.nodes.kind(*expr) {
                                    NodeKind::Expr(Expr::Lambda(..)) => {
                                        for (_, o) in &mut new_offsets {
                                            *o += 1;
                                        }
                                        new_offsets.insert(name.clone(), 0);
                                        out.push(Instruction::Alloc);
                                        self.generate_expr(*expr, &new_offsets, out);
                                        out.push(Instruction::Update(0));
                                    }
                                    _ => {
                                        self.generate_expr(*expr, &new_offsets, out);
                                        for (_, o) in &mut new_offsets {
                                            *o += 1;
                                        }
                                        new_offsets.insert(name.clone(), 0);
                                    }
                                };
                                binds += 1;
                            }
                            NodeKind::Expr(..) => self.generate_expr(*n, &new_offsets, out),
                            _ => unreachable!(),
                        }
                    }
                    out.push(Instruction::Slide(binds))
                }
                Expr::Lambda(..) => {
                    let lambda_name = self.create_lambda_name();
                    let mut lambda_offsets = FxHashMap::default();
                    let mut lambda_arity = 0;
                    let mut lambda_insts = Vec::new();
                    let mut lambda_body = node;
                    while let NodeKind::Expr(Expr::Lambda(l, r)) = self.nodes.kind(lambda_body) {
                        lambda_offsets.insert(l.clone(), lambda_offsets.len());
                        lambda_body = *r;
                        lambda_arity += 1;
                    }
                    let mut free_vars = Vec::new();
                    self.collect_free_vars(
                        lambda_body,
                        &mut lambda_offsets.iter().map(|(p, _)| p.clone()).collect(),
                        &mut free_vars,
                    );
                    lambda_arity += free_vars.len();
                    for v in &free_vars {
                        for (_, o) in &mut lambda_offsets {
                            *o += 1;
                        }
                        match self.nodes.kind(*v) {
                            NodeKind::Expr(Expr::Identifier(id)) => {
                                lambda_offsets.insert(id.clone(), 0);
                            }
                            _ => unreachable!(),
                        }
                    }
                    self.generate_expr(lambda_body, &lambda_offsets, &mut lambda_insts);
                    lambda_insts.push(Instruction::Update(lambda_arity));
                    if lambda_arity > 0 {
                        lambda_insts.push(Instruction::Pop(lambda_arity));
                    }
                    lambda_insts.push(Instruction::Unwind);
                    self.symbols.insert(lambda_name.clone(), lambda_insts);
                    self.symbols_arity.insert(lambda_name.clone(), lambda_arity);
                    let mut offsets = offsets.clone();
                    for v in &free_vars {
                        self.generate_expr(*v, &offsets, out);
                        for (_, o) in &mut offsets {
                            *o += 1;
                        }
                    }
                    out.push(Instruction::PushGlobal(lambda_name, lambda_arity));
                    for _ in &free_vars {
                        out.push(Instruction::MkAp)
                    }
                }
            },
            _ => unreachable!(),
        }
    }

    fn generate_match(
        &mut self,
        r#match: Match,
        mut offsets: FxHashMap<String, usize>,
        mut frames: Vec<(usize, usize, usize)>,
    ) -> Vec<Instruction> {
        if let Some((row, body)) = r#match.first()
            && row.is_empty()
        {
            let mut out = Vec::new();
            self.generate_expr(*body, &offsets, &mut out);
            return out;
        }

        let mut out = Vec::new();

        if let Some((offset, field, arity)) = frames.pop() {
            out.push(Instruction::Push(offset - (arity - field)));
            if field + 1 < arity {
                frames.push((offset, field + 1, arity))
            }
        } else {
            let mut insts = Vec::new();
            self.generate_expr(r#match.scrutinee(), &offsets, &mut insts);
            out.extend(insts);
        }

        out.push(Instruction::Eval);

        for offset in offsets.values_mut() {
            *offset += 1;
        }

        for (offset, ..) in frames.iter_mut() {
            *offset += 1
        }

        for id in r#match.identifiers() {
            offsets.insert(id, 0);
        }

        let default = r#match.default();
        let default = (!default.is_empty()).then(|| {
            self.generate_match(default, offsets.clone(), frames.clone())
                .into_iter()
                .chain(std::iter::once(Instruction::Slide(1)))
                .collect()
        });

        let used_ctors = r#match.used_constructors();
        let ctors = r#match.constructors();

        let arms = ctors
            .into_iter()
            .filter(|(n, _)| used_ctors.contains(n.as_str()))
            .map(|(name, (tag, arity))| {
                let new_match = r#match.specialize(&name, arity);
                let new_offsets = offsets
                    .iter()
                    .map(|(n, o)| (n.clone(), *o + arity))
                    .collect();
                let new_frames = frames
                    .iter()
                    .copied()
                    .map(|(o, f, a)| (o + arity, f, a))
                    .chain(std::iter::once((arity, 0, arity)).filter(|_| arity > 0))
                    .collect();
                (
                    tag,
                    std::iter::once(Instruction::Unpack(arity))
                        .chain(self.generate_match(new_match, new_offsets, new_frames))
                        .chain(std::iter::once(Instruction::Slide(arity + 1)))
                        .collect(),
                )
            })
            .collect();

        out.push(Instruction::Case(arms, default));
        out
    }
}

pub fn generate(ctx: &NodeArena) -> FxHashMap<String, Vec<Instruction>> {
    let mut generator = IRGenerator::new(ctx);
    generator.generate();
    generator.symbols
}
