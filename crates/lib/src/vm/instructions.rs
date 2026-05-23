use fxhash::FxHashMap;

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
