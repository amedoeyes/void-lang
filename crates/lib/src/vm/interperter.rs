use std::fmt::{Display, Formatter};

use fxhash::FxHashMap;

use crate::vm::instructions::Instruction;

#[derive(Debug, Clone)]
pub enum Value {
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
pub struct Address(usize);

#[derive(Debug, Clone)]
pub enum Global {
    Builtin,
    Super(Vec<Instruction>),
}

#[derive(Debug, Clone)]
pub enum Node {
    Integer(i64),
    Application(Address, Address),
    Global(String, usize, Global),
    Constructor(usize, Vec<Address>),
    Indirection(Address),
}

#[derive(Debug)]
pub struct GMachine {
    pub pc: usize,
    pub instructions: Vec<Instruction>,
    pub globals: FxHashMap<String, Address>,
    pub heap: Vec<Node>,
    pub stack: Vec<Address>,
    pub dump: Vec<(Vec<Address>, Vec<Instruction>)>,
    pub output: Vec<Value>,
}

impl GMachine {
    pub fn new() -> Self {
        Self {
            pc: 0,
            instructions: Vec::new(),
            globals: FxHashMap::default(),
            heap: Vec::new(),
            stack: Vec::new(),
            dump: Vec::new(),
            output: Vec::new(),
        }
    }

    pub fn alloc(&mut self, node: Node) -> Address {
        let addr = self.heap.len();
        self.heap.push(node);
        Address(addr)
    }

    pub fn is_whnf(&self, addr: Address) -> bool {
        match &self.heap[addr.0] {
            Node::Integer(..) => true,
            Node::Application(..) => false,
            Node::Global(..) => false,
            Node::Indirection(..) => false,
            Node::Constructor(..) => true,
        }
    }

    pub fn force(&mut self, addr: Address) -> Address {
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

    pub fn run(&mut self) {
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
                    let addr = self.alloc(Node::Integer(i));
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
                    self.heap[root.0] = Node::Indirection(addr);
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
                    let app = self.alloc(Node::Application(l, r));
                    self.stack.push(app);
                    self.pc += 1;
                }
                Instruction::Pack(tag, arity) => {
                    let args = self
                        .stack
                        .drain(self.stack.len() - arity..self.stack.len())
                        .rev()
                        .collect();
                    let cons = self.alloc(Node::Constructor(tag, args));
                    self.stack.push(cons);
                    self.pc += 1;
                }
                Instruction::Split(n) => {
                    let addr = self.stack.pop().unwrap();
                    match &self.heap[addr.0] {
                        Node::Constructor(_, args) => {
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
                        Node::Constructor(tag, _) => {
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
                        Node::Integer(i) if i == 1 => {
                            self.instructions.splice(self.pc + 1..self.pc + 1, t);
                        }
                        Node::Integer(i) if i == 0 => {
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
                        Node::Integer(..) | Node::Constructor(..) => {
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
                        Node::Application(l, r) => {
                            // println!("Application({:?}, {:?})", self.heap[l.0], self.heap[r.0]);
                            self.stack.push(l);
                        }
                        Node::Indirection(addr) => {
                            // println!("Indirection({:?})", self.heap[addr.0]);
                            *self.stack.last_mut().unwrap() = addr;
                        }
                        Node::Global(name, arity, global) => {
                            // println!("Global({name}, {arity}, {global:?})");
                            if self.stack.len() <= arity {
                                self.pc += 1;
                            } else {
                                let mut args = self.stack[..arity]
                                    .iter()
                                    .try_fold(Vec::with_capacity(arity), |mut acc, a| {
                                        if let Node::Application(_, r) = self.heap[a.0] {
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
                                            if let (Node::Integer(l), Node::Integer(r)) = (lhs, rhs)
                                            {
                                                let result_addr = self.alloc(Node::Integer(l + r));
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
                                            if let (Node::Integer(l), Node::Integer(r)) = (lhs, rhs)
                                            {
                                                let result_addr = self.alloc(Node::Integer(l - r));
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
                                            if let (Node::Integer(l), Node::Integer(r)) = (lhs, rhs)
                                            {
                                                let result_addr = self.alloc(Node::Integer(l * r));
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
                                            if let (Node::Integer(l), Node::Integer(r)) = (lhs, rhs)
                                            {
                                                let result_addr =
                                                    self.alloc(Node::Integer((l == r) as i64));
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
                                            if let (Node::Integer(l), Node::Integer(r)) = (lhs, rhs)
                                            {
                                                let result_addr =
                                                    self.alloc(Node::Integer((l <= r) as i64));
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
                                            if let (Node::Integer(l), Node::Integer(r)) = (lhs, rhs)
                                            {
                                                let result_addr =
                                                    self.alloc(Node::Integer((l >= r) as i64));
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
                                            if let (Node::Integer(l), Node::Integer(r)) = (lhs, rhs)
                                            {
                                                let result_addr =
                                                    self.alloc(Node::Integer((l < r) as i64));
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
                                            if let (Node::Integer(l), Node::Integer(r)) = (lhs, rhs)
                                            {
                                                let result_addr =
                                                    self.alloc(Node::Integer((l > r) as i64));
                                                self.stack.push(result_addr);
                                            }
                                        }
                                        "&&" => {
                                            let lhs_addr = args.pop().unwrap();
                                            let lhs_addr = self.force(lhs_addr);
                                            let lhs = &self.heap[lhs_addr.0];
                                            if let Node::Integer(l) = lhs
                                                && *l == 1
                                            {
                                                let rhs_addr = args.pop().unwrap();
                                                let rhs_addr = self.force(rhs_addr);
                                                let rhs = &self.heap[rhs_addr.0];
                                                if let Node::Integer(r) = rhs
                                                    && *r == 1
                                                {
                                                    let result_addr = self.alloc(Node::Integer(1));
                                                    self.stack.push(result_addr);
                                                } else {
                                                    let result_addr = self.alloc(Node::Integer(0));
                                                    self.stack.push(result_addr);
                                                }
                                            } else {
                                                let result_addr = self.alloc(Node::Integer(0));
                                                self.stack.push(result_addr);
                                            }
                                        }
                                        "||" => {
                                            let lhs_addr = args.pop().unwrap();
                                            let lhs_addr = self.force(lhs_addr);
                                            let lhs = &self.heap[lhs_addr.0];
                                            if let Node::Integer(l) = lhs
                                                && *l == 1
                                            {
                                                let result_addr = self.alloc(Node::Integer(1));
                                                self.stack.push(result_addr);
                                            } else {
                                                let rhs_addr = args.pop().unwrap();
                                                let rhs_addr = self.force(rhs_addr);
                                                let rhs = &self.heap[rhs_addr.0];
                                                if let Node::Integer(r) = rhs
                                                    && *r == 1
                                                {
                                                    let result_addr = self.alloc(Node::Integer(1));
                                                    self.stack.push(result_addr);
                                                } else {
                                                    let result_addr = self.alloc(Node::Integer(0));
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
                        Node::Integer(i) => self.output.push(Value::Integer(*i)),
                        Node::Constructor(_, args) => {
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
