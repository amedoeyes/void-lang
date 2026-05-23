use fxhash::{FxHashMap, FxHashSet};

use crate::{
    context::{Context, Node, NodeId},
    expr::{Expr, Pattern},
    type_system::Type,
    vm::instructions::Instruction,
};

#[derive(Debug, Clone)]
pub struct Super {
    pub name: String,
    pub arity: usize,
    pub instructions: Vec<Instruction>,
}

impl Super {
    fn new(name: String) -> Self {
        Self {
            name,
            arity: 0,
            instructions: Vec::new(),
        }
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
            Expr::Boolean(b) => out.push(Instruction::PushInt(*b as i64)),
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
            Expr::Condition { cond, then, alt } => {
                compile_expr(&ctx, *cond, types, offsets, out);
                out.push(Instruction::Eval);
                let mut then_insts = Vec::new();
                compile_expr(&ctx, *then, types, &offsets, &mut then_insts);
                let mut alt_insts = Vec::new();
                compile_expr(&ctx, *alt, types, &offsets, &mut alt_insts);
                out.push(Instruction::Cond(then_insts, alt_insts));
            }
            other => todo!("{other:?}"),
        },
        _ => unreachable!(),
    }
}

pub fn compile(ctx: &Context, nodes: &[NodeId]) -> Vec<Super> {
    let mut ir = Vec::new();

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
                    let mut sc = Super::new(cons.clone());
                    sc.arity = args.len();
                    for (i, (j, _)) in args.iter().enumerate().rev().enumerate() {
                        sc.instructions.push(Instruction::Push(j + i));
                    }
                    sc.instructions.push(Instruction::Pack(i + 1, sc.arity));
                    sc.instructions.push(Instruction::Update(sc.arity));
                    sc.instructions.push(Instruction::Pop(sc.arity));
                    sc.instructions.push(Instruction::Unwind);
                    ir.push(sc);
                }
            }
            Node::Bind(name, expr) => {
                let mut sc = Super::new(name);
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
                ir.push(sc);
            }
            _ => continue,
        }
    }
    ir
}
