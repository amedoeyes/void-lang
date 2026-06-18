use std::fmt::{Display, Formatter};

use fxhash::FxHashMap;

#[derive(Debug, Clone)]
pub enum Instruction {
    PushInt(i64),
    Alloc(usize),
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
            Instruction::Alloc(n) => write!(f, "Alloc {n}"),
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
