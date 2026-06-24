pub mod instructions;
pub mod interperter;

use fxhash::{FxHashMap, FxHashSet};

use crate::{
    context::{Context, Node, NodeId},
    expr::{Expr, Pattern},
    ir::instructions::Instruction,
    type_system::Type,
};

#[derive(Debug)]
pub struct IRGenerator<'a> {
    pub context: &'a Context,
    pub symbols: FxHashMap<String, Vec<Instruction>>,
    pub symbols_arity: FxHashMap<String, usize>,
    pub symbols_alias: FxHashMap<String, String>,
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
            symbols_arity: FxHashMap::default(),
            symbols_alias: FxHashMap::default(),
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

        for module in &modules {
            for node in module {
                match self.context.get_node(*node) {
                    Node::Type(_, _, constructors) => {
                        for (cons, args) in constructors {
                            self.symbols_arity.insert(cons.clone(), args.len());
                        }
                    }
                    Node::Primitive(name, _, link_name) => {
                        if let Some(ty) = self.context.get_type(*node) {
                            self.symbols_alias.insert(name.clone(), link_name.clone());
                            self.symbols_arity.insert(name.clone(), ty.arity());
                            self.symbols_arity.insert(link_name.clone(), ty.arity());
                        }
                    }
                    Node::Bind(name, ..) => {
                        if let Some(ty) = self.context.get_type(*node) {
                            self.symbols_arity.insert(name.clone(), ty.arity());
                        }
                    }
                    _ => continue,
                }
            }
        }

        for module in modules {
            for node in module {
                match self.context.get_node(node) {
                    Node::Type(_, _, constructors) => {
                        for (i, (cons, args)) in constructors.iter().enumerate() {
                            let mut insts = Vec::new();
                            let arity = args.len();
                            insts.push(Instruction::Pack(i, arity));
                            insts.push(Instruction::Update(0));
                            insts.push(Instruction::Unwind);
                            self.symbols.insert(cons.clone(), insts);
                        }
                    }
                    Node::Bind(name, .., expr) => {
                        if let Some(ty) = self.context.get_type(*expr) {
                            let arity = ty.arity();
                            let mut insts = Vec::new();
                            match self.context.get_node(*expr) {
                                Node::Expr(Expr::Lambda { .. }) => {
                                    let mut offsets = FxHashMap::default();
                                    let mut node = *expr;
                                    while let Node::Expr(Expr::Lambda { param, body }) =
                                        self.context.get_node(node)
                                    {
                                        offsets.insert(param.clone(), offsets.len());
                                        node = *body;
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
                            insts.extend([
                                Instruction::Update(arity),
                                Instruction::Pop(arity),
                                Instruction::Unwind,
                            ]);
                            self.symbols.insert(name.clone(), insts);
                        }
                    }
                    _ => continue,
                }
            }
        }
    }

    fn collect_free_vars(&self, node: NodeId, bound: &mut Vec<String>, out: &mut Vec<NodeId>) {
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
                for (pat, body) in branches {
                    let original_len = bound.len();
                    pat.collect_bound_vars(bound);
                    self.collect_free_vars(*body, bound, out);
                    bound.truncate(original_len);
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
                                    insts.push(Instruction::Unpack(subpatterns.len()));
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
                Expr::Block(nodes) => {
                    let mut new_offsets = offsets.clone();
                    let mut binds = 0;
                    for n in nodes {
                        match self.context.get_node(*n) {
                            Node::Bind(name, .., expr) => {
                                match self.context.get_node(*expr) {
                                    Node::Expr(Expr::Lambda { .. }) => {
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
                            Node::Expr(..) => self.generate_expr(*n, &new_offsets, out),
                            _ => unreachable!(),
                        }
                    }
                    out.push(Instruction::Slide(binds))
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
                        &mut lambda_offsets.iter().map(|(p, _)| p.clone()).collect(),
                        &mut free_vars,
                    );
                    lambda_arity += free_vars.len();
                    for v in &free_vars {
                        for (_, o) in &mut lambda_offsets {
                            *o += 1;
                        }
                        match self.context.get_node(*v) {
                            Node::Expr(Expr::Identifier(id)) => {
                                lambda_offsets.insert(id.clone(), 0);
                            }
                            _ => unreachable!(),
                        }
                    }
                    self.generate_expr(lambda_body, &lambda_offsets, &mut lambda_insts);
                    lambda_insts.push(Instruction::Update(lambda_arity));
                    lambda_insts.push(Instruction::Pop(lambda_arity));
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
                other => todo!("{other:?}"),
            },
            _ => unreachable!(),
        }
    }
}

pub fn generate(ctx: &Context) -> FxHashMap<String, Vec<Instruction>> {
    let mut generator = IRGenerator::new(ctx);
    generator.generate();
    generator.symbols
}
