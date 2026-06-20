use std::io;

use fxhash::FxHashMap;

use crate::ir::instructions::Instruction;

pub mod x86_64;

type Result<T> = std::result::Result<T, io::Error>;

pub trait Backend<'a> {
    fn emit_push_int(&mut self, int: i64) -> Result<()>;
    fn emit_push_global(&mut self, name: &str, arity: usize) -> Result<()>;
    fn emit_alloc(&mut self) -> Result<()>;
    fn emit_push(&mut self, n: usize) -> Result<()>;
    fn emit_pop(&mut self, n: usize) -> Result<()>;
    fn emit_slide(&mut self, n: usize) -> Result<()>;
    fn emit_update(&mut self, n: usize) -> Result<()>;
    fn emit_mkap(&mut self) -> Result<()>;
    fn emit_pack(&mut self, tag: usize, arity: usize) -> Result<()>;
    fn emit_unpack(&mut self, n: usize) -> Result<()>;
    fn emit_case(&mut self, branches: &FxHashMap<usize, Vec<Instruction>>) -> Result<()>;
    fn emit_unwind(&mut self) -> Result<()>;
    fn emit_eval(&mut self) -> Result<()>;
    fn emit_instruction(&mut self, inst: &Instruction) -> Result<()>;
    fn emit_primitives(&mut self) -> Result<()>;
    fn emit(self) -> Result<()>;
}
