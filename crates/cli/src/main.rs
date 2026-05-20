use clap::{Parser, Subcommand, crate_name, crate_version};
use core::fmt::{self, Display, Formatter};
use fxhash::{FxHashMap, FxHashSet};
use std::{collections::HashMap, env, fs, io, path::PathBuf};
use void::{
    context::{Context, Node, NodeId},
    error,
    expr::{Expr, Pattern},
    lexer::{Lexer, Token},
    modules,
    parser::{self, parse},
    type_system::{Type, infer},
};

#[derive(Debug, Clone)]
struct Super {
    name: String,
    arity: usize,
    instructions: Vec<Instruction>,
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

#[derive(Debug, Clone)]
enum Value {
    Unit,
    Integer(i64),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => writeln!(f, "()"),
            Value::Integer(i) => writeln!(f, "{i}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Address(usize);

#[derive(Debug, Clone)]
enum Global {
    Builtin,
    Super(Vec<Instruction>),
}

#[derive(Debug, Clone)]
enum GNode {
    Integer(i64),
    Application(Address, Address),
    Global(String, usize, Global),
    Constructor(usize, Vec<Address>),
    Indirection(Address),
}

#[derive(Debug, Clone)]
enum Instruction {
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

#[derive(Debug)]
struct GMachine {
    pc: usize,
    instructions: Vec<Instruction>,
    globals: HashMap<String, Address>,
    heap: Vec<GNode>,
    stack: Vec<Address>,
    dump: Vec<(Vec<Address>, Vec<Instruction>)>,
    output: Vec<Value>,
}

impl GMachine {
    fn new() -> Self {
        Self {
            pc: 0,
            instructions: Vec::new(),
            globals: HashMap::new(),
            heap: Vec::new(),
            stack: Vec::new(),
            dump: Vec::new(),
            output: Vec::new(),
        }
    }

    fn alloc(&mut self, node: GNode) -> Address {
        let addr = self.heap.len();
        self.heap.push(node);
        Address(addr)
    }

    fn is_whnf(&self, addr: Address) -> bool {
        match &self.heap[addr.0] {
            GNode::Integer(..) => true,
            GNode::Application(..) => false,
            GNode::Global(..) => false,
            GNode::Indirection(..) => false,
            GNode::Constructor(..) => true,
        }
    }

    fn force(&mut self, addr: Address) -> Address {
        if self.is_whnf(addr) {
            return addr;
        }

        let saved_stack = std::mem::take(&mut self.stack);
        let saved_dump = std::mem::take(&mut self.dump);
        let saved_instructions = std::mem::take(&mut self.instructions);
        let saved_pc = self.pc;
        let saved_output = std::mem::take(&mut self.output);

        self.stack = Vec::from([addr]);
        self.instructions = Vec::from([Instruction::Eval]);
        self.pc = 0;

        self.run();

        let result = self.stack[0];

        self.stack = saved_stack;
        self.dump = saved_dump;
        self.instructions = saved_instructions;
        self.pc = saved_pc;
        self.output = saved_output;

        result
    }

    fn run(&mut self) {
        loop {
            if self.pc >= self.instructions.len() {
                break;
            }

            let inst = self.instructions[self.pc].clone();

            // println!("------");
            // println!("pc: {}", self.pc);
            //     println!("instructions: {:?}", self.instructions);
            // println!(
            //     "stack: {:?}",
            //     self.stack
            //         .iter()
            //         .map(|a| &self.heap[a.0])
            //         .collect::<Vec<_>>()
            // );
            // println!("heap: {:?}", self.heap);
            //     println!("dump: {:?}", self.dump);
            //     println!("output: {:?}", self.output);

            // println!("inst: {:?}", inst);

            match inst {
                Instruction::PushInt(i) => {
                    let addr = self.alloc(GNode::Integer(i));
                    self.stack.push(addr);
                    self.pc += 1;
                }
                Instruction::PushGlobal(name) => {
                    let addr = *self.globals.get(&name).unwrap();
                    self.stack.push(addr);
                    self.pc += 1;
                }
                Instruction::Push(n) => {
                    let addr = self.stack[self.stack.len() - 1 - n];
                    self.stack.push(addr);
                    self.pc += 1;
                }
                Instruction::Pop(n) => {
                    self.stack.truncate(self.stack.len() - n);
                    self.pc += 1;
                }
                Instruction::Update(n) => {
                    let addr = self.stack.pop().unwrap();
                    let root = self.stack[self.stack.len() - 1 - n];
                    self.heap[root.0] = GNode::Indirection(addr);
                    self.pc += 1;
                }
                Instruction::Slide(n) => {
                    self.stack
                        .drain(self.stack.len() - n - 1..self.stack.len() - 1);
                    self.pc += 1;
                }
                Instruction::MkAp => {
                    let l = self.stack.pop().unwrap();
                    let r = self.stack.pop().unwrap();
                    let app = self.alloc(GNode::Application(l, r));
                    self.stack.push(app);
                    self.pc += 1;
                }
                Instruction::Pack(tag, arity) => {
                    let args = self
                        .stack
                        .drain(self.stack.len() - arity..self.stack.len())
                        .rev()
                        .collect();
                    let cons = self.alloc(GNode::Constructor(tag, args));
                    self.stack.push(cons);
                    self.pc += 1;
                }
                Instruction::Split(n) => {
                    let addr = self.stack.pop().unwrap();
                    match &self.heap[addr.0] {
                        GNode::Constructor(_, args) => {
                            for a in args[..n].iter().rev() {
                                self.stack.push(*a);
                            }
                        }
                        _ => todo!(),
                    }
                    self.pc += 1;
                }
                Instruction::Case(mut branches) => {
                    let addr = self.stack.last().unwrap();
                    match &self.heap[addr.0] {
                        GNode::Constructor(tag, _) => {
                            self.instructions
                                .splice(self.pc + 1..self.pc + 1, branches.remove(tag).unwrap());
                        }
                        _ => todo!(),
                    }
                    self.pc += 1;
                }
                Instruction::Cond(t, f) => {
                    let addr = self.stack.pop().unwrap();
                    match self.heap[addr.0] {
                        GNode::Integer(i) if i == 1 => {
                            self.instructions.splice(self.pc + 1..self.pc + 1, t);
                        }
                        GNode::Integer(i) if i == 0 => {
                            self.instructions.splice(self.pc + 1..self.pc + 1, f);
                        }
                        _ => todo!(),
                    }
                    self.pc += 1;
                }
                Instruction::Eval => {
                    let top = *self.stack.last().unwrap();
                    if !self.is_whnf(top) {
                        self.dump.push((
                            self.stack[..self.stack.len().saturating_sub(1)].to_vec(),
                            self.instructions[self.pc + 1..].to_vec(),
                        ));
                        self.pc = 0;
                        self.instructions = Vec::from([Instruction::Unwind]);
                        self.stack = Vec::from([top]);
                    } else {
                        self.pc += 1;
                    }
                }
                Instruction::Unwind => {
                    let top = *self.stack.last().unwrap();

                    match self.heap[top.0].clone() {
                        GNode::Integer(..) | GNode::Constructor(..) => {
                            if !self.dump.is_empty() {
                                let (stack, instructions) = self.dump.pop().unwrap();
                                self.stack = stack;
                                self.stack.push(top);
                                self.instructions = instructions;
                                self.pc = 0;
                            } else {
                                self.pc += 1;
                            }
                        }
                        GNode::Application(l, r) => {
                            // println!("Application({:?}, {:?})", self.heap[l.0], self.heap[r.0]);
                            self.stack.push(l);
                        }
                        GNode::Indirection(addr) => {
                            // println!("Indirection({:?})", self.heap[addr.0]);
                            *self.stack.last_mut().unwrap() = addr;
                        }
                        GNode::Global(name, arity, global) => {
                            // println!("Global({name}, {arity}, {global:?})");
                            if self.stack.len() <= arity {
                                self.pc += 1;
                            } else {
                                let mut args = self.stack[..arity]
                                    .iter()
                                    .try_fold(Vec::with_capacity(arity), |mut acc, a| {
                                        if let GNode::Application(_, r) = self.heap[a.0] {
                                            acc.push(r);
                                            Ok(acc)
                                        } else {
                                            Err(String::from("expected application"))
                                        }
                                    })
                                    .unwrap();

                                // println!(
                                //     "fun: {name}, args: {:?}",
                                //     args.iter().map(|a| &self.heap[a.0]).collect::<Vec<_>>()
                                // );

                                let redex_root = self.stack[self.stack.len() - 1 - arity];
                                match global {
                                    Global::Builtin => match name.as_str() {
                                        "println" => {
                                            let val_addr = args.pop().unwrap();
                                            let val_addr = self.force(val_addr);
                                            let val = &self.heap[val_addr.0];
                                            println!("{:?}", val);
                                            self.stack.push(val_addr);
                                        }
                                        "+" => {
                                            let lhs_addr = args.pop().unwrap();
                                            let rhs_addr = args.pop().unwrap();
                                            let lhs_addr = self.force(lhs_addr);
                                            let rhs_addr = self.force(rhs_addr);
                                            let lhs = &self.heap[lhs_addr.0];
                                            let rhs = &self.heap[rhs_addr.0];
                                            if let (GNode::Integer(l), GNode::Integer(r)) =
                                                (lhs, rhs)
                                            {
                                                let result_addr = self.alloc(GNode::Integer(l + r));
                                                self.stack.push(result_addr);
                                            }
                                        }
                                        "-" => {
                                            let lhs_addr = args.pop().unwrap();
                                            let rhs_addr = args.pop().unwrap();
                                            let lhs_addr = self.force(lhs_addr);
                                            let rhs_addr = self.force(rhs_addr);
                                            let lhs = &self.heap[lhs_addr.0];
                                            let rhs = &self.heap[rhs_addr.0];
                                            if let (GNode::Integer(l), GNode::Integer(r)) =
                                                (lhs, rhs)
                                            {
                                                let result_addr = self.alloc(GNode::Integer(l - r));
                                                self.stack.push(result_addr);
                                            }
                                        }
                                        "*" => {
                                            let lhs_addr = args.pop().unwrap();
                                            let rhs_addr = args.pop().unwrap();
                                            let lhs_addr = self.force(lhs_addr);
                                            let rhs_addr = self.force(rhs_addr);
                                            let lhs = &self.heap[lhs_addr.0];
                                            let rhs = &self.heap[rhs_addr.0];
                                            if let (GNode::Integer(l), GNode::Integer(r)) =
                                                (lhs, rhs)
                                            {
                                                let result_addr = self.alloc(GNode::Integer(l * r));
                                                self.stack.push(result_addr);
                                            }
                                        }
                                        "==" => {
                                            let lhs_addr = args.pop().unwrap();
                                            let rhs_addr = args.pop().unwrap();
                                            let lhs_addr = self.force(lhs_addr);
                                            let rhs_addr = self.force(rhs_addr);
                                            let lhs = &self.heap[lhs_addr.0];
                                            let rhs = &self.heap[rhs_addr.0];
                                            if let (GNode::Integer(l), GNode::Integer(r)) =
                                                (lhs, rhs)
                                            {
                                                let result_addr =
                                                    self.alloc(GNode::Integer((l == r) as i64));
                                                self.stack.push(result_addr);
                                            }
                                        }
                                        "<=" => {
                                            let lhs_addr = args.pop().unwrap();
                                            let rhs_addr = args.pop().unwrap();
                                            let lhs_addr = self.force(lhs_addr);
                                            let rhs_addr = self.force(rhs_addr);
                                            let lhs = &self.heap[lhs_addr.0];
                                            let rhs = &self.heap[rhs_addr.0];
                                            if let (GNode::Integer(l), GNode::Integer(r)) =
                                                (lhs, rhs)
                                            {
                                                let result_addr =
                                                    self.alloc(GNode::Integer((l <= r) as i64));
                                                self.stack.push(result_addr);
                                            }
                                        }
                                        ">=" => {
                                            let lhs_addr = args.pop().unwrap();
                                            let rhs_addr = args.pop().unwrap();
                                            let lhs_addr = self.force(lhs_addr);
                                            let rhs_addr = self.force(rhs_addr);
                                            let lhs = &self.heap[lhs_addr.0];
                                            let rhs = &self.heap[rhs_addr.0];
                                            if let (GNode::Integer(l), GNode::Integer(r)) =
                                                (lhs, rhs)
                                            {
                                                let result_addr =
                                                    self.alloc(GNode::Integer((l >= r) as i64));
                                                self.stack.push(result_addr);
                                            }
                                        }
                                        "<" => {
                                            let lhs_addr = args.pop().unwrap();
                                            let rhs_addr = args.pop().unwrap();
                                            let lhs_addr = self.force(lhs_addr);
                                            let rhs_addr = self.force(rhs_addr);
                                            let lhs = &self.heap[lhs_addr.0];
                                            let rhs = &self.heap[rhs_addr.0];
                                            if let (GNode::Integer(l), GNode::Integer(r)) =
                                                (lhs, rhs)
                                            {
                                                let result_addr =
                                                    self.alloc(GNode::Integer((l < r) as i64));
                                                self.stack.push(result_addr);
                                            }
                                        }
                                        ">" => {
                                            let lhs_addr = args.pop().unwrap();
                                            let rhs_addr = args.pop().unwrap();
                                            let lhs_addr = self.force(lhs_addr);
                                            let rhs_addr = self.force(rhs_addr);
                                            let lhs = &self.heap[lhs_addr.0];
                                            let rhs = &self.heap[rhs_addr.0];
                                            if let (GNode::Integer(l), GNode::Integer(r)) =
                                                (lhs, rhs)
                                            {
                                                let result_addr =
                                                    self.alloc(GNode::Integer((l > r) as i64));
                                                self.stack.push(result_addr);
                                            }
                                        }
                                        "&&" => {
                                            let lhs_addr = args.pop().unwrap();
                                            let lhs_addr = self.force(lhs_addr);
                                            let lhs = &self.heap[lhs_addr.0];
                                            if let GNode::Integer(l) = lhs
                                                && *l == 1
                                            {
                                                let rhs_addr = args.pop().unwrap();
                                                let rhs_addr = self.force(rhs_addr);
                                                let rhs = &self.heap[rhs_addr.0];
                                                if let GNode::Integer(r) = rhs
                                                    && *r == 1
                                                {
                                                    let result_addr = self.alloc(GNode::Integer(1));
                                                    self.stack.push(result_addr);
                                                } else {
                                                    let result_addr = self.alloc(GNode::Integer(0));
                                                    self.stack.push(result_addr);
                                                }
                                            } else {
                                                let result_addr = self.alloc(GNode::Integer(0));
                                                self.stack.push(result_addr);
                                            }
                                        }
                                        "||" => {
                                            let lhs_addr = args.pop().unwrap();
                                            let lhs_addr = self.force(lhs_addr);
                                            let lhs = &self.heap[lhs_addr.0];
                                            if let GNode::Integer(l) = lhs
                                                && *l == 1
                                            {
                                                let result_addr = self.alloc(GNode::Integer(1));
                                                self.stack.push(result_addr);
                                            } else {
                                                let rhs_addr = args.pop().unwrap();
                                                let rhs_addr = self.force(rhs_addr);
                                                let rhs = &self.heap[rhs_addr.0];
                                                if let GNode::Integer(r) = rhs
                                                    && *r == 1
                                                {
                                                    let result_addr = self.alloc(GNode::Integer(1));
                                                    self.stack.push(result_addr);
                                                } else {
                                                    let result_addr = self.alloc(GNode::Integer(0));
                                                    self.stack.push(result_addr);
                                                }
                                            }
                                        }
                                        _ => unreachable!(),
                                    },
                                    Global::Super(instructions) => {
                                        self.pc = 0;
                                        self.instructions = instructions;
                                        self.stack = Vec::with_capacity(arity + 1);
                                        self.stack.push(redex_root);
                                        self.stack.extend(args);
                                    }
                                }
                            }
                        }
                    }
                }
                Instruction::Print => {
                    let result_addr = self.stack.pop().unwrap();
                    match &self.heap[result_addr.0] {
                        GNode::Integer(i) => self.output.push(Value::Integer(*i)),
                        GNode::Constructor(_, args) => {
                            let mut new_insts = Vec::new();
                            for a in args.iter().rev() {
                                self.stack.push(*a);
                                new_insts.push(Instruction::Eval);
                                new_insts.push(Instruction::Print);
                            }
                            self.instructions.extend(new_insts);
                        }
                        _ => unreachable!(),
                    }
                    self.pc += 1;
                }
            }

            //     println!("instructions: {:?}", self.instructions);
            //     println!(
            //         "stack: {:?}",
            //         self.stack
            //             .iter()
            //             .map(|a| &self.heap[a.0])
            //             .collect::<Vec<_>>()
            //     );
            //     println!("heap: {:?}", self.heap);
            //     println!("dump: {:?}", self.dump);
            //     println!("output: {:?}", self.output);
        }

        // println!();
    }
}

fn compile_expr(
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

fn compile(ctx: &Context, nodes: &[NodeId]) -> Vec<Super> {
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

#[derive(Debug)]
enum Error {
    Void(error::Error),
    Io(io::Error),
}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Error::Io(value)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Error::Io(err) => err.fmt(f),
            Error::Void(err) => err.fmt(f),
        }
    }
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Parser)]
#[command(name = crate_name!())]
#[command(version = crate_version!())]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Lex { file: PathBuf },
    Parse { file: PathBuf },
    Type { file: PathBuf },
    // Repl { file: Option<PathBuf> },
    Compile { file: PathBuf },
    Run { file: PathBuf },
}

fn main() {
    let res = match Cli::parse().command {
        Commands::Lex { file } => lex_cmd(&file),
        Commands::Parse { file } => parse_cmd(&file),
        Commands::Type { file } => type_cmd(&file),
        // Commands::Repl { file } => repl_cmd(file.as_ref()),
        Commands::Compile { file } => compile_cmd(&file),
        Commands::Run { file } => run_cmd(&file),
    };

    if let Err(e) = res {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}

fn compile_cmd(source_path: &PathBuf) -> Result<()> {
    let mut ctx = Context::new();

    let parent_dir = source_path
        .parent()
        .ok_or_else(|| Error::Void(error::Error::InvalidPath(source_path.clone())))?;

    let mut visited_modules = FxHashSet::default();
    visited_modules.insert(PathBuf::from(source_path));

    let contents = fs::read_to_string(source_path)?;
    let nodes = match parse(&mut ctx, &contents) {
        Ok(nodes) => modules::resolve_imports(
            &mut ctx,
            &nodes,
            parent_dir,
            &mut visited_modules,
            &mut FxHashSet::default(),
        )
        .map_err(Error::Void)?,
        Err(err) => {
            return Err(Error::Void(error::Error::Syntax(
                source_path.clone(),
                contents,
                Box::new(err),
            )));
        }
    };

    if let Err(err) = infer(&mut ctx, &nodes) {
        return Err(Error::Void(error::Error::Type(
            source_path.clone(),
            contents,
            Box::new(err),
        )));
    }

    let ir = compile(&ctx, &nodes);

    println!("{ir:#?}");

    Ok(())
}

fn run_cmd(source_path: &PathBuf) -> Result<()> {
    let mut ctx = Context::new();

    let parent_dir = source_path
        .parent()
        .ok_or_else(|| Error::Void(error::Error::InvalidPath(source_path.clone())))?;

    let mut visited_modules = FxHashSet::default();
    visited_modules.insert(PathBuf::from(source_path));

    let contents = fs::read_to_string(source_path)?;
    let nodes = match parse(&mut ctx, &contents) {
        Ok(nodes) => modules::resolve_imports(
            &mut ctx,
            &nodes,
            parent_dir,
            &mut visited_modules,
            &mut FxHashSet::default(),
        )
        .map_err(Error::Void)?,
        Err(err) => {
            return Err(Error::Void(error::Error::Syntax(
                source_path.clone(),
                contents,
                Box::new(err),
            )));
        }
    };

    if let Err(err) = infer(&mut ctx, &nodes) {
        return Err(Error::Void(error::Error::Type(
            source_path.clone(),
            contents,
            Box::new(err),
        )));
    }

    let ir = compile(&ctx, &nodes);

    let mut machine = GMachine::new();
    machine.instructions = Vec::from([
        Instruction::PushGlobal("main".into()),
        Instruction::Eval,
        Instruction::Print,
    ]);

    for s in ir {
        let global = machine.alloc(GNode::Global(
            s.name.clone(),
            s.arity,
            Global::Super(s.instructions),
        ));
        machine.globals.insert(s.name.clone(), global);
    }

    let println_global = machine.alloc(GNode::Global("println".into(), 1, Global::Builtin));
    let add_global = machine.alloc(GNode::Global("+".into(), 2, Global::Builtin));
    let sub_global = machine.alloc(GNode::Global("-".into(), 2, Global::Builtin));
    let mul_global = machine.alloc(GNode::Global("*".into(), 2, Global::Builtin));
    let eq_global = machine.alloc(GNode::Global("==".into(), 2, Global::Builtin));
    let le_global = machine.alloc(GNode::Global("<=".into(), 2, Global::Builtin));
    let ge_global = machine.alloc(GNode::Global(">=".into(), 2, Global::Builtin));
    let lt_global = machine.alloc(GNode::Global("<".into(), 2, Global::Builtin));
    let gt_global = machine.alloc(GNode::Global(">".into(), 2, Global::Builtin));
    let and_global = machine.alloc(GNode::Global("&&".into(), 2, Global::Builtin));
    let or_global = machine.alloc(GNode::Global("||".into(), 2, Global::Builtin));
    machine.globals.insert("println".into(), println_global);
    machine.globals.insert("+".into(), add_global);
    machine.globals.insert("-".into(), sub_global);
    machine.globals.insert("*".into(), mul_global);
    machine.globals.insert("==".into(), eq_global);
    machine.globals.insert("<=".into(), le_global);
    machine.globals.insert(">=".into(), ge_global);
    machine.globals.insert("<".into(), lt_global);
    machine.globals.insert(">".into(), gt_global);
    machine.globals.insert("&&".into(), and_global);
    machine.globals.insert("||".into(), or_global);

    machine.run();

    println!("result: {:?}", machine.output);

    Ok(())
}

fn lex_cmd(source_path: &PathBuf) -> Result<()> {
    let contents = fs::read_to_string(source_path)?;
    let mut lexer = Lexer::new(&contents);

    loop {
        match lexer.next_token() {
            Ok((token, span)) => {
                println!(
                    "{}:{}:{}: {:?}",
                    source_path.display(),
                    span.start.line,
                    span.end.column,
                    token
                );
                if token == Token::Eof {
                    break;
                }
            }
            Err(err) => {
                return Err(Error::Void(error::Error::Syntax(
                    source_path.clone(),
                    contents,
                    Box::new(parser::Error::Lexer(err)),
                )));
            }
        }
    }

    Ok(())
}

fn parse_cmd(source_path: &PathBuf) -> Result<()> {
    let mut ctx = Context::new();

    let parent_dir = source_path
        .parent()
        .ok_or_else(|| Error::Void(error::Error::InvalidPath(source_path.clone())))?;

    let mut visited_modules = FxHashSet::default();
    visited_modules.insert(PathBuf::from(source_path));

    let contents = fs::read_to_string(source_path)?;
    let nodes = match parse(&mut ctx, &contents) {
        Ok(nodes) => modules::resolve_imports(
            &mut ctx,
            &nodes,
            parent_dir,
            &mut visited_modules,
            &mut FxHashSet::default(),
        )
        .map_err(Error::Void)?,
        Err(err) => {
            return Err(Error::Void(error::Error::Syntax(
                source_path.clone(),
                contents,
                Box::new(err),
            )));
        }
    };

    for node in nodes {
        println!("{}", node.display(&ctx));
    }

    Ok(())
}

fn type_cmd(source_path: &PathBuf) -> Result<()> {
    let mut ctx = Context::new();

    let parent_dir = source_path
        .parent()
        .ok_or_else(|| Error::Void(error::Error::InvalidPath(source_path.clone())))?;

    let mut visited_modules = FxHashSet::default();
    visited_modules.insert(PathBuf::from(source_path));

    let contents = fs::read_to_string(source_path)?;
    let nodes = match parse(&mut ctx, &contents) {
        Ok(nodes) => modules::resolve_imports(
            &mut ctx,
            &nodes,
            parent_dir,
            &mut visited_modules,
            &mut FxHashSet::default(),
        )
        .map_err(Error::Void)?,
        Err(err) => {
            return Err(Error::Void(error::Error::Syntax(
                source_path.clone(),
                contents,
                Box::new(err),
            )));
        }
    };

    if let Err(err) = infer(&mut ctx, &nodes) {
        return Err(Error::Void(error::Error::Type(
            source_path.clone(),
            contents,
            Box::new(err),
        )));
    }

    for node in nodes {
        match ctx.get_node(node) {
            Node::TypeExpr(..) => todo!(),
            Node::Expr(..) => {
                println!(
                    "{} : {}",
                    node.display(&ctx),
                    ctx.get_type(node).as_ref().unwrap()
                )
            }
            Node::Type(..) => {
                println!(
                    "{} : {}",
                    node.display(&ctx),
                    ctx.get_type(node).as_ref().unwrap()
                )
            }
            Node::Bind(name, _) => println!("{} : {}", name, ctx.get_type(node).as_ref().unwrap()),
            Node::Import(..) => continue,
        }
    }

    Ok(())
}

// fn repl_cmd(source_path: Option<&PathBuf>) -> Result<()> {
//     let mut ctx = Context::new();

//     let mut rl = DefaultEditor::new().expect("could not initialize line editor");

//     let mut nodes = Vec::new();

//     if let Some(source_path) = source_path {
//         let parent_dir = source_path
//             .parent()
//             .ok_or_else(|| Error::Void(error::Error::InvalidPath(source_path.clone())))?;

//         let mut visited_modules = FxHashSet::default();
//         visited_modules.insert(PathBuf::from(source_path));

//         let contents = fs::read_to_string(source_path)?;
//         nodes.extend(match parse(&mut ctx, &contents) {
//             Ok(nodes) => modules::resolve_imports(
//                 &mut ctx,
//                 &nodes,
//                 parent_dir,
//                 &mut visited_modules,
//                 &mut FxHashSet::default(),
//             )
//             .map_err(Error::Void)?,
//             Err(err) => {
//                 return Err(Error::Void(error::Error::Syntax(
//                     source_path.clone(),
//                     contents,
//                     Box::new(err),
//                 )));
//             }
//         });

//         if let Err(err) = infer(&mut ctx, &nodes) {
//             return Err(Error::Void(error::Error::Type(
//                 source_path.clone(),
//                 contents,
//                 Box::new(err),
//             )));
//         }
//     }

//     let cwd = env::current_dir()?;
//     let parent_dir = cwd
//         .parent()
//         .ok_or_else(|| Error::Void(error::Error::InvalidPath(cwd.clone())))?;

//     loop {
//         let input = match rl.readline("> ") {
//             Ok(line) => {
//                 rl.add_history_entry(&line)
//                     .expect("could not add history entry");
//                 line
//             }
//             Err(rustyline::error::ReadlineError::Interrupted) => {
//                 continue;
//             }
//             Err(_) => {
//                 break;
//             }
//         };

//         if input.trim().is_empty() {
//             continue;
//         }

//         nodes.extend(match parse(&mut ctx, &input) {
//             Ok(nodes) => modules::resolve_imports(
//                 &mut ctx,
//                 &nodes,
//                 parent_dir,
//                 &mut FxHashSet::default(),
//                 &mut FxHashSet::default(),
//             )
//             .map_err(Error::Void)?,

//             Err(err) => {
//                 match err {
//                     parser::Error::Lexer(lexer::Error::InvalidToken(span))
//                     | parser::Error::Lexer(lexer::Error::Unterminated(span, _))
//                     | parser::Error::Lexer(lexer::Error::EmptyChar(span))
//                     | parser::Error::Lexer(lexer::Error::InvalidChar(span))
//                     | parser::Error::Lexer(lexer::Error::InvalidEscapeChar(span))
//                     | parser::Error::UnexpectedToken(_, (_, span)) => {
//                         println!(
//                             "{}:{}: {}",
//                             span.start.line,
//                             span.start.column,
//                             &err.to_string()
//                         );
//                     }
//                 }
//                 continue;
//             }
//         });

//         if let Err(err) = infer(&mut ctx, &nodes) {
//             match err {
//                 type_system::Error::TypeMismatch(ty1, ty2, span) => {
//                     println!(
//                         "{}:{}: expected type '{ty1}', but found '{ty2}'",
//                         span.start.line, span.start.column
//                     );
//                 }
//                 type_system::Error::InfiniteType(ty, span) => {
//                     println!(
//                         "{}:{}: infinite type '{ty}'",
//                         span.start.line, span.start.column
//                     );
//                 }
//                 type_system::Error::UnknownIdentifier(id, span) => {
//                     println!(
//                         "{}:{}: unknown identifier '{id}'",
//                         span.start.line, span.start.column
//                     );
//                 }
//                 type_system::Error::UnknownOperator(op, span) => {
//                     println!(
//                         "{}:{}: unknown operator '({op})'",
//                         span.start.line, span.start.column
//                     );
//                 }
//                 type_system::Error::NoInstance(cons, ty, span) => {
//                     println!(
//                         "{}:{}: No '{cons}' instance for type '{ty}'",
//                         span.start.line, span.start.column
//                     );
//                 }
//             }
//             nodes.pop();
//             continue;
//         }

//         if let Node::Expr(_) = ctx.get_node(*nodes.last().unwrap()) {
//             let value = match evaluate(&ctx, &nodes) {
//                 Ok(value) => value,
//                 Err(err) => {
//                     match err {
//                         eval::Error::DivisionByZero(span) => println!(
//                             "{}:{}: division by zero",
//                             span.start.line, span.start.column
//                         ),
//                         eval::Error::EmptyList(span) => {
//                             println!("{}:{}: list is empty", span.start.line, span.start.column)
//                         }
//                         eval::Error::IO(message, span) => {
//                             println!("{}:{}: {message}", span.start.line, span.start.column)
//                         }
//                     }
//                     continue;
//                 }
//             };

//             println!("{}", value.display(&ctx));
//             nodes.pop();
//         }
//     }

//     Ok(())
// }
