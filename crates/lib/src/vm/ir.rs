use std::fmt::{Display, Formatter};

use fxhash::{FxHashMap, FxHashSet};

use crate::{
    context::{Context, Node, NodeId},
    expr::{Expr, Pattern},
    type_system::Type,
};

#[derive(Debug, Clone)]
pub enum Instruction {
    PushInt(i64),
    Push(usize),
    PushGlobal(String),
    Pop(usize),
    Update(usize),
    Slide(usize),
    MkAp,
    Pack(usize, usize),
    Split(usize),
    Case(FxHashMap<usize, Vec<Instruction>>),
    Cond(Vec<Instruction>, Vec<Instruction>),
    Eval,
    Unwind,
    Print,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Instruction::PushInt(i) => write!(f, "PushInt {i}"),
            Instruction::Push(n) => write!(f, "Push {n}"),
            Instruction::PushGlobal(name) => write!(f, "PushGlobal \"{name}\""),
            Instruction::Pop(n) => write!(f, "Pop {n}"),
            Instruction::Update(n) => write!(f, "Update {n}"),
            Instruction::Slide(n) => write!(f, "Slide {n}"),
            Instruction::MkAp => write!(f, "MkAp"),
            Instruction::Pack(t, a) => write!(f, "Pack {t} {a}"),
            Instruction::Split(n) => write!(f, "Split {n}"),
            Instruction::Case(branches) => {
                write!(
                    f,
                    "Case {{ {} }}",
                    branches
                        .iter()
                        .map(|(t, insts)| {
                            format!(
                                "{} => {{ {} }}",
                                t,
                                insts
                                    .iter()
                                    .map(|i| format!("{i}"))
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Instruction::Cond(t_branch, f_branch) => {
                write!(
                    f,
                    "Cond {{ true => {}, false = {} }}",
                    t_branch
                        .iter()
                        .map(|i| format!("{i}"))
                        .collect::<Vec<_>>()
                        .join(", "),
                    f_branch
                        .iter()
                        .map(|i| format!("{i}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            Instruction::Eval => write!(f, "Eval"),
            Instruction::Unwind => write!(f, "Unwind"),
            Instruction::Print => write!(f, "Print"),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Super {
    pub arity: usize,
    pub instructions: Vec<Instruction>,
}

impl Super {
    pub fn new() -> Self {
        Self::default()
    }
}

pub fn compile_expr(
    ctx: &Context,
    node: NodeId,
    types: &FxHashMap<String, FxHashMap<String, usize>>,
    offsets: &FxHashMap<String, usize>,
    out: &mut Vec<Instruction>,
) {
    match ctx.get_node(node) {
        Node::Expr(expr) => match expr {
            Expr::Integer(i) => out.push(Instruction::PushInt(*i)),
            Expr::Constructor(cons) => out.push(Instruction::PushGlobal(cons.clone())),
            Expr::Identifier(id) => {
                if let Some(offset) = offsets.get(id) {
                    out.push(Instruction::Push(*offset));
                } else {
                    out.push(Instruction::PushGlobal(id.clone()));
                }
            }
            Expr::Application { func, arg } => {
                compile_expr(&ctx, *arg, types, offsets, out);
                compile_expr(
                    &ctx,
                    *func,
                    types,
                    &offsets.iter().map(|(k, v)| (k.clone(), v + 1)).collect(),
                    out,
                );
                out.push(Instruction::MkAp);
            }
            Expr::Match(scrutinee, branches) => {
                let consts = match ctx.get_type(*scrutinee) {
                    Some(Type::Adt(name, _)) => types.get(name).unwrap(),
                    _ => todo!(),
                };
                let mut consts_used = FxHashSet::default();
                compile_expr(&ctx, *scrutinee, types, offsets, out);
                out.push(Instruction::Eval);
                let mut compiled_branches = FxHashMap::default();
                for (pattern, body) in branches {
                    match pattern {
                        Pattern::Wildcard => {
                            for cons in consts
                                .keys()
                                .collect::<FxHashSet<_>>()
                                .difference(&consts_used)
                            {
                                let mut insts = Vec::new();
                                compile_expr(
                                    &ctx,
                                    *body,
                                    types,
                                    &offsets.iter().map(|(k, v)| (k.clone(), v + 1)).collect(),
                                    &mut insts,
                                );
                                insts.push(Instruction::Slide(1));
                                compiled_branches.insert(*consts.get(*cons).unwrap(), insts);
                            }
                        }
                        Pattern::Identifier(id) => {
                            for cons in consts
                                .keys()
                                .collect::<FxHashSet<_>>()
                                .difference(&consts_used)
                            {
                                let mut insts = Vec::new();
                                compile_expr(
                                    &ctx,
                                    *body,
                                    types,
                                    &offsets
                                        .iter()
                                        .map(|(k, v)| (k.clone(), v + 1))
                                        .chain(std::iter::once((id.clone(), offsets.len())))
                                        .collect(),
                                    &mut insts,
                                );
                                insts.push(Instruction::Slide(1));
                                compiled_branches.insert(*consts.get(*cons).unwrap(), insts);
                            }
                        }
                        Pattern::Constructor(name, subpatterns) => {
                            consts_used.insert(name);
                            let mut insts = Vec::new();
                            if !subpatterns.is_empty() {
                                insts.push(Instruction::Split(subpatterns.len()));
                            }
                            compile_expr(
                                &ctx,
                                *body,
                                types,
                                &offsets
                                    .iter()
                                    .map(|(k, v)| (k.clone(), v + subpatterns.len().max(1)))
                                    .chain(subpatterns.iter().enumerate().filter_map(|(i, p)| {
                                        if let Pattern::Identifier(id) = p {
                                            Some((id.clone(), i))
                                        } else {
                                            None
                                        }
                                    }))
                                    .collect::<FxHashMap<_, _>>(),
                                &mut insts,
                            );
                            insts.push(Instruction::Slide(subpatterns.len().max(1)));
                            compiled_branches.insert(*consts.get(name).unwrap(), insts);
                        }
                    }
                }
                out.push(Instruction::Case(compiled_branches));
            }
            other => todo!("{other:?}"),
        },
        _ => unreachable!(),
    }
}

pub fn compile(ctx: &Context, nodes: &[NodeId]) -> FxHashMap<String, Super> {
    let mut ir = FxHashMap::default();

    let types = nodes
        .iter()
        .filter_map(|n| match ctx.get_node(*n) {
            Node::Type(name, _, constructors) => Some((
                name.clone(),
                constructors
                    .iter()
                    .enumerate()
                    .map(|(i, (c, _))| (c.clone(), i + 1))
                    .collect::<FxHashMap<_, _>>(),
            )),
            _ => None,
        })
        .collect::<FxHashMap<_, _>>();

    for node in nodes {
        match ctx.get_node(*node).clone() {
            Node::Type(_, _, constructors) => {
                for (i, (cons, args)) in constructors.iter().enumerate() {
                    let mut sc = Super::new();
                    sc.arity = args.len();
                    for (i, (j, _)) in args.iter().enumerate().rev().enumerate() {
                        sc.instructions.push(Instruction::Push(j + i));
                    }
                    sc.instructions.push(Instruction::Pack(i + 1, sc.arity));
                    sc.instructions.push(Instruction::Update(sc.arity));
                    sc.instructions.push(Instruction::Pop(sc.arity));
                    sc.instructions.push(Instruction::Unwind);
                    ir.insert(cons.clone(), sc);
                }
            }
            Node::Bind(name, expr) => {
                let mut sc = Super::new();
                match ctx.get_node(expr).clone() {
                    Node::Expr(Expr::Lambda { .. }) => {
                        let mut offsets = FxHashMap::default();
                        let mut node = expr;
                        while let Node::Expr(Expr::Lambda { param, body }) = ctx.get_node(node) {
                            offsets.insert(param.clone(), offsets.len());
                            node = *body;
                            sc.arity += 1;
                        }
                        compile_expr(ctx, node, &types, &offsets, &mut sc.instructions);
                    }
                    _ => compile_expr(
                        ctx,
                        expr,
                        &types,
                        &FxHashMap::default(),
                        &mut sc.instructions,
                    ),
                }
                sc.instructions.push(Instruction::Update(sc.arity));
                sc.instructions.push(Instruction::Pop(sc.arity));
                sc.instructions.push(Instruction::Unwind);
                ir.insert(name, sc);
            }
            _ => continue,
        }
    }
    ir
}
