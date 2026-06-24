use std::fmt::{Display, Formatter};

use fxhash::FxHashMap;

use crate::ir::Instruction;

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
pub enum Node {
    Integer(i64),
    Constructor(usize, Vec<Address>),
    Indirection(Address),
    Application(Address, Address),
    Global(String, usize),
}

#[derive(Debug)]
pub struct GMachine<'a> {
    pub pc: usize,
    pub instructions: Vec<&'a Instruction>,
    pub symbols: &'a FxHashMap<String, Vec<Instruction>>,
    pub stack: Vec<Address>,
    pub heap: Vec<Node>,
    pub dump: Vec<(usize, usize, Vec<&'a Instruction>)>,
}

impl<'a> GMachine<'a> {
    pub fn new(
        symbols: &'a FxHashMap<String, Vec<Instruction>>,
        instructions: &[&'a Instruction],
    ) -> Self {
        Self {
            pc: 0,
            instructions: instructions.to_vec(),
            symbols,
            heap: Vec::from([Node::Indirection(Address(0))]),
            stack: Vec::new(),
            dump: Vec::new(),
        }
    }

    pub fn alloc(&mut self, node: Node) -> Address {
        let addr = self.heap.len();
        self.heap.push(node);
        Address(addr)
    }

    pub fn is_whnf(&self, addr: Address) -> bool {
        match &self.heap[addr.0] {
            Node::Integer(..) | Node::Constructor(..) => true,
            _ => false,
        }
    }

    pub fn force(&mut self, addr: Address) -> Address {
        if self.is_whnf(addr) {
            return addr;
        }

        self.dump.push((
            self.stack.len().saturating_sub(1),
            self.pc,
            self.instructions.clone(),
        ));

        self.stack.push(addr);
        self.pc = 0;
        self.instructions = Vec::from([&Instruction::Unwind]);

        self.run();

        self.stack.pop().unwrap()
    }

    pub fn run(&mut self) {
        loop {
            if self.pc >= self.instructions.len() {
                break;
            }

            match self.instructions[self.pc] {
                Instruction::PushInt(i) => {
                    let addr = self.alloc(Node::Integer(*i));
                    self.stack.push(addr);
                    self.pc += 1;
                }
                Instruction::Alloc => {
                    let addr = self.alloc(Node::Indirection(Address(0)));
                    self.stack.push(addr);
                    self.pc += 1;
                }
                Instruction::PushGlobal(name, arity) => {
                    let addr = self.alloc(Node::Global(name.clone(), *arity));
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
                    let cons = self.alloc(Node::Constructor(*tag, args));
                    self.stack.push(cons);
                    self.pc += 1;
                }
                Instruction::Unpack(n) => {
                    let addr = self.stack.pop().unwrap();
                    match &self.heap[addr.0] {
                        Node::Constructor(_, args) => {
                            for a in args[..*n].iter().rev() {
                                self.stack.push(*a);
                            }
                        }
                        _ => todo!(),
                    }
                    self.pc += 1;
                }
                Instruction::Case(branches) => {
                    let addr = self.stack.last().unwrap();
                    match &self.heap[addr.0] {
                        Node::Constructor(tag, _) => {
                            self.instructions
                                .splice(self.pc + 1..self.pc + 1, branches.get(tag).unwrap());
                        }
                        _ => todo!(),
                    }
                    self.pc += 1;
                }
                Instruction::Eval => {
                    let top = *self.stack.last().unwrap();
                    if !self.is_whnf(top) {
                        self.dump.push((
                            self.stack.len().saturating_sub(1),
                            self.pc,
                            self.instructions.clone(),
                        ));
                        self.pc = 0;
                        self.instructions = Vec::from([&Instruction::Unwind]);
                    } else {
                        self.pc += 1;
                    }
                }
                Instruction::Unwind => {
                    let top = *self.stack.last().unwrap();

                    match self.heap[top.0].clone() {
                        Node::Integer(..) | Node::Constructor(..) => {
                            let (stack, pc, instructions) = self.dump.pop().unwrap();
                            self.stack.truncate(stack);
                            self.stack.push(top);
                            self.instructions = instructions;
                            self.pc = pc;
                        }
                        Node::Application(l, _) => self.stack.push(l),
                        Node::Indirection(addr) => *self.stack.last_mut().unwrap() = addr,
                        Node::Global(name, arity) => {
                            if arity > 0 {
                                for i in (self.stack.len() - arity..self.stack.len()).rev() {
                                    match self.heap[self.stack[i - 1].0] {
                                        Node::Application(_, r) => self.stack[i] = r,
                                        _ => unreachable!(),
                                    }
                                }
                            }
                            match self.symbols.get(&name) {
                                Some(insts) => {
                                    self.pc = 0;
                                    self.instructions = insts.iter().map(|i| i).collect();
                                }
                                None => panic!("symbol not found '{name}'"),
                            }
                        }
                    }
                }
            }
        }
    }

    pub fn print(&mut self, addr: Address) {
        let addr = self.force(addr);
        match self.heap[addr.0].clone() {
            Node::Integer(i) => print!("{i}"),
            Node::Constructor(tag, args) => {
                print!("<{tag}>");
                if !args.is_empty() {
                    let mut args = args.iter();
                    print!("(");
                    self.print(*args.next().unwrap());
                    for a in args {
                        print!(", ");
                        self.print(*a);
                    }
                    print!(")");
                }
            }
            _ => unreachable!(),
        }
    }

    pub fn println(&mut self, addr: Address) {
        self.print(addr);
        println!();
    }
}
