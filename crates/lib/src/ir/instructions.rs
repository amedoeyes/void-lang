use std::fmt::{Display, Formatter};

use fxhash::FxHashMap;

#[derive(Debug, Clone)]
pub enum Instruction {
    PushUnit,
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
    Case(FxHashMap<usize, Vec<Instruction>>),
    Eval,
    Unwind,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Instruction::PushUnit => write!(f, "PUSHUNIT"),
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
            Instruction::Eval => write!(f, "EVAL"),
            Instruction::Unwind => write!(f, "UNWIND"),
        }
    }
}
