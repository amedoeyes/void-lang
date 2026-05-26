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
            Instruction::Eval => write!(f, "Eval"),
            Instruction::Unwind => write!(f, "Unwind"),
            Instruction::Print => write!(f, "Print"),
        }
    }
}

#[derive(Debug)]
pub struct IRGenerator<'a> {
    pub context: &'a Context,
    pub symbols: FxHashMap<String, (usize, Vec<Instruction>)>,
    pub type_consts: FxHashMap<String, FxHashMap<String, usize>>,
    pub lambda_counter: usize,
}

impl<'a> IRGenerator<'a> {
    pub fn new(context: &'a Context) -> Self {
        let type_consts = context
            .nodes()
            .iter()
            .filter_map(|n| match n {
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

        Self {
            context,
            symbols: FxHashMap::default(),
            type_consts,
            lambda_counter: 0,
        }
    }

    pub fn generate(&mut self) {
        let modules = self
            .context
            .nodes()
            .iter()
            .filter_map(|n| match n {
                Node::Module(nodes) => Some(nodes.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();

        for module in modules {
            for node in module {
                match self.context.get_node(node) {
                    Node::Type(_, _, constructors) => {
                        for (i, (cons, args)) in constructors.iter().enumerate() {
                            let mut insts = Vec::new();
                            let arity = args.len();
                            insts.push(Instruction::Pack(i + 1, arity));
                            insts.push(Instruction::Update(0));
                            insts.push(Instruction::Unwind);
                            self.symbols.insert(cons.clone(), (arity, insts));
                        }
                    }
                    Node::Bind(name, expr) => {
                        let mut insts = Vec::new();
                        let mut arity = 0;
                        match self.context.get_node(*expr).clone() {
                            Node::Expr(Expr::Lambda { .. }) => {
                                let mut offsets = FxHashMap::default();
                                let mut node = *expr;
                                while let Node::Expr(Expr::Lambda { param, body }) =
                                    self.context.get_node(node)
                                {
                                    offsets.insert(param.clone(), offsets.len());
                                    node = *body;
                                    arity += 1;
                                }
                                self.generate_expr(node, &offsets, &mut insts)
                            }
                            _ => self.generate_expr(*expr, &FxHashMap::default(), &mut insts),
                        }
                        insts.push(Instruction::Update(arity));
                        insts.push(Instruction::Pop(arity));
                        insts.push(Instruction::Unwind);
                        self.symbols.insert(name.clone(), (arity, insts));
                    }
                    _ => continue,
                }
            }
        }
    }

    fn collect_free_vars(&self, node: NodeId, bound: &FxHashSet<String>, out: &mut Vec<NodeId>) {
        match self.context.get_node(node) {
            Node::Expr(Expr::Identifier(id)) => {
                if !bound.contains(id) {
                    out.push(node);
                }
            }
            Node::Expr(Expr::Application { func, arg }) => {
                self.collect_free_vars(*func, bound, out);
                self.collect_free_vars(*arg, bound, out);
            }
            Node::Expr(Expr::Match(scrutinee, branches)) => {
                self.collect_free_vars(*scrutinee, bound, out);
                for (_, body) in branches {
                    self.collect_free_vars(*body, bound, out);
                }
            }
            _ => {}
        }
    }

    fn create_lambda_name(&mut self) -> String {
        let name = format!("__lambda_{}", self.lambda_counter);
        self.lambda_counter += 1;
        name
    }

    pub fn generate_expr(
        &mut self,
        node: NodeId,
        offsets: &FxHashMap<String, usize>,
        out: &mut Vec<Instruction>,
    ) {
        match self.context.get_node(node) {
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
                    self.generate_expr(*arg, offsets, out);
                    self.generate_expr(
                        *func,
                        &offsets.iter().map(|(k, v)| (k.clone(), v + 1)).collect(),
                        out,
                    );
                    out.push(Instruction::MkAp);
                }
                Expr::Match(scrutinee, branches) => {
                    let consts = match self.context.get_type(*scrutinee) {
                        Some(Type::Adt(name, _)) => self.type_consts.get(name).unwrap().clone(),
                        _ => todo!(),
                    };
                    let mut consts_used = FxHashSet::default();
                    self.generate_expr(*scrutinee, offsets, out);
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
                                    self.generate_expr(
                                        *body,
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
                                    self.generate_expr(
                                        *body,
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
                                self.generate_expr(
                                    *body,
                                    &offsets
                                        .iter()
                                        .map(|(k, v)| (k.clone(), v + subpatterns.len().max(1)))
                                        .chain(subpatterns.iter().enumerate().filter_map(
                                            |(i, p)| {
                                                if let Pattern::Identifier(id) = p {
                                                    Some((id.clone(), i))
                                                } else {
                                                    None
                                                }
                                            },
                                        ))
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
                Expr::Lambda { .. } => {
                    let lambda_name = self.create_lambda_name();
                    let mut lambda_offsets = FxHashMap::default();
                    let mut lambda_arity = 0;
                    let mut lambda_insts = Vec::new();
                    let mut lambda_body = node;
                    while let Node::Expr(Expr::Lambda { param, body }) =
                        self.context.get_node(lambda_body)
                    {
                        lambda_offsets.insert(param.clone(), lambda_offsets.len());
                        lambda_body = *body;
                        lambda_arity += 1;
                    }
                    let mut free_vars = Vec::new();
                    self.collect_free_vars(
                        lambda_body,
                        &lambda_offsets.iter().map(|(p, _)| p.clone()).collect(),
                        &mut free_vars,
                    );
                    lambda_arity += free_vars.len();
                    for (i, v) in free_vars.iter().enumerate() {
                        for (_, o) in &mut lambda_offsets {
                            *o += 1;
                        }
                        match self.context.get_node(*v) {
                            Node::Expr(Expr::Identifier(id)) => {
                                lambda_offsets.insert(id.clone(), i);
                            }
                            _ => unreachable!(),
                        }
                    }
                    self.generate_expr(lambda_body, &lambda_offsets, &mut lambda_insts);
                    lambda_insts.push(Instruction::Update(lambda_arity));
                    lambda_insts.push(Instruction::Pop(lambda_arity));
                    lambda_insts.push(Instruction::Unwind);
                    self.symbols
                        .insert(lambda_name.clone(), (lambda_arity, lambda_insts));
                    for v in &free_vars {
                        self.generate_expr(*v, &offsets, out)
                    }
                    out.push(Instruction::PushGlobal(lambda_name));
                    for _ in &free_vars {
                        out.push(Instruction::MkAp)
                    }
                }
                other => todo!("{other:?}"),
            },
            _ => unreachable!(),
        }
    }
}

pub fn generate(ctx: &Context) -> FxHashMap<String, (usize, Vec<Instruction>)> {
    let mut generator = IRGenerator::new(ctx);
    generator.generate();
    generator.symbols
}
