use std::io::Write;

use fxhash::FxHashMap;

use crate::{
    codegen::{Backend, Result},
    ir::instructions::Instruction,
};

const RUNTIME: &str = r#"
default rel

section .bss
stack_base resq 1024
dump_base resq 2048
heap_start resq 4096
heap_free_ptr resq 1

TAG_INT equ 0
TAG_CNST equ 1
TAG_IND equ 2
TAG_APP equ 3
TAG_GLBL equ 4

section .text
global  _start

%macro stack_push 1
	sub r15, 8
	mov [r15], %1
%endmacro

%macro stack_pop 1
	mov %1, [r15]
	add r15, 8
%endmacro

%macro dump_push 2
	sub r14, 16
	mov [r14], %1
	mov [r14+8], %2
%endmacro

%macro dump_pop 2
	mov %1, [r14]
	mov %2, [r14+8]
	add r14, 16
%endmacro

unwind:
	mov rdi, [r15]
	mov rsi, [rdi]

	jmp [.jump_table+(rsi*8)]
	.jump_table:
		dq .tag_int
		dq .tag_cnst
		dq .tag_ind
		dq .tag_app
		dq .tag_glbl

	.tag_int:
	.tag_cnst:
		dump_pop rsi, rdx
		mov r15, rdx
		stack_push rdi
		jmp rsi

	.tag_ind:
		mov rdi, [rdi+8]
		mov qword [r15], rdi
		jmp unwind

	.tag_app:
		mov rdi, [rdi+8]
		stack_push rdi
		jmp unwind

	.tag_glbl:
		mov rsi, [rdi+8]
		mov rdx, [rdi+16]

		test rdx, rdx
		jz .done

		xor rdi, rdi
		.loop:
			cmp rdi, rdx
			je .done
			mov rcx, [r15+(rdi+1)*8]
			mov rcx, [rcx+16]
			mov [r15+rdi*8], rcx
			inc rdi
			jmp .loop
		.done:
		jmp rsi

force:
	stack_push rdi

	mov rdi, .return
	mov rsi, r15
	add rsi, 8
	dump_push rdi, rsi

	jmp unwind

	.return:
	stack_pop rax
	ret

print:
	call force
	mov r8, rax

	cmp [r8], TAG_INT
	je .tag_int
	cmp [r8], TAG_CNST
	je .tag_cnst

	jmp .done

	.tag_int:
		mov rdi, [r8+8]
		call print_int
		jmp .done

	.tag_cnst:
		mov rdi, '<'
		call print_char

		mov rdi, [r8+8]
		call print_int

		mov rdi, '>'
		call print_char

		mov rdi, [r8+16]
		test rdi, rdi
		jz .done

		mov rdi, '('
		call print_char

		push r8
		mov rdi, [r8+24]
		call print
		pop r8

		mov r9, 1
		.loop:
			cmp r9, [r8+16]
			je .loop_end

			mov rdi, ','
			call print_char

			mov rdi, ' '
			call print_char

			push r8
			push r9
			mov rdi, [r8+24+r9*8]
			call print
			pop r9
			pop r8

			inc r9
			jmp .loop

		.loop_end:

		mov rdi, ')'
		call print_char

		jmp .done

	.done:

	ret

println:
	call print
	mov rdi, 10
	call print_char

runtime_init:
	mov r15, stack_base + 1024*8
	mov r14, dump_base + 2048*8
	mov rdi, heap_start
	mov [heap_free_ptr], rdi
	ret

heap_alloc:
	mov rax, [heap_free_ptr]
	add [heap_free_ptr], rdi
	ret

heap_make_int:
	mov  rsi, rdi
	mov  rdi, 16
	call heap_alloc
	mov  qword [rax], TAG_INT
	mov  qword [rax+8], rsi
	ret

heap_make_ind:
	mov  rsi, rdi
	mov  rdi, 16
	call heap_alloc
	mov  qword [rax], TAG_IND
	mov  qword [rax+8], rsi
	ret

heap_make_app:
	mov  rdx, rdi
	mov  rdi, 24
	call heap_alloc
	mov  qword [rax], TAG_APP
	mov  qword [rax+8], rdx
	mov  qword [rax+16], rsi
	ret

heap_make_global:
	mov  rdx, rdi
	mov  rdi, 24
	call heap_alloc
	mov  qword [rax], TAG_GLBL
	mov  qword [rax+8], rdx
	mov  qword [rax+16], rsi
	ret

heap_make_constr:
	mov rcx, rsi
	mov rsi, rdi

	mov rdi, rcx
	shl rdi, 3
	add rdi, 24
	call heap_alloc

	mov qword [rax], TAG_CNST
	mov qword [rax+8], rsi
	mov qword [rax+16], rcx

	lea rdi, [rax+24]
	mov rsi, rdx
	cld
	rep movsq

	ret

print_int:
	mov rax, rdi

	lea rsi, [rsp-1]

	mov rcx, 10

	.convert:
		xor rdx, rdx
		div rcx
		add dl, '0'
		mov [rsi], dl
		dec rsi
		test rax, rax
		jnz .convert

	inc rsi
	mov rdx, rsp
	sub rdx, rsi

	mov rax, 1
	mov rdi, 1
	syscall
	ret

print_char:
	mov [rsp-1], dil
	mov rax, 1
	mov rdi, 1
	lea rsi, [rsp-1]
	mov rdx, 1
	syscall
	ret

print_str:
	mov rdx, rsi
	mov rsi, rdi
	mov rax, 1
	mov rdi, 1
	syscall
	ret

_start:
	call runtime_init

	; PUSHGLOBAL main, 0
	mov rdi, main
	mov rsi, 0
	call heap_make_global
	stack_push rax

	; EVAL
	mov rdi, .return
	mov rsi, r15
	add rsi, 8
	dump_push rdi, rsi
	jmp unwind
	.return:

	stack_pop rdi
	call println

	mov rdi, 0
	mov rax, 60
	syscall
"#;

pub struct X86_64<'a, 'b, W: Write> {
    writer: &'a mut W,
    symbols: &'b FxHashMap<String, (usize, Vec<Instruction>)>,
    case_counter: usize,
    eval_counter: usize,
}

impl<'a, 'b, W: Write> X86_64<'a, 'b, W> {
    pub fn new(
        writer: &'a mut W,
        symbols: &'b FxHashMap<String, (usize, Vec<Instruction>)>,
    ) -> Self {
        Self {
            writer,
            symbols,
            case_counter: 0,
            eval_counter: 0,
        }
    }
}

impl<'a, 'b, W: Write> Backend<'b> for X86_64<'a, 'b, W> {
    fn emit_push_int(&mut self, int: i64) -> Result<()> {
        writeln!(self.writer, "	; PUSHINT {int}")?;
        writeln!(self.writer, "	mov rdi, {int}")?;
        writeln!(self.writer, "	call heap_make_int")?;
        writeln!(self.writer, "	stack_push rax")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_push_global(&mut self, name: &str, arity: usize) -> Result<()> {
        writeln!(self.writer, "	; PUSHGLOBAL {name}")?;
        writeln!(self.writer, "	mov rdi, {name}")?;
        writeln!(self.writer, "	mov rsi, {arity}")?;
        writeln!(self.writer, "	call heap_make_global")?;
        writeln!(self.writer, "	stack_push rax")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_alloc(&mut self) -> Result<()> {
        writeln!(self.writer, "	; ALLOC")?;
        writeln!(self.writer, "	mov rdi, 0")?;
        writeln!(self.writer, "	call heap_make_ind")?;
        writeln!(self.writer, "	stack_push rax")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_push(&mut self, n: usize) -> Result<()> {
        writeln!(self.writer, "	; PUSH {n}")?;
        writeln!(self.writer, "	mov rdi, [r15+{}]", n * 8)?;
        writeln!(self.writer, "	stack_push rdi")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_pop(&mut self, n: usize) -> Result<()> {
        writeln!(self.writer, "	; POP {n}")?;
        writeln!(self.writer, "	add r15, {}", n * 8)?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_slide(&mut self, n: usize) -> Result<()> {
        writeln!(self.writer, "	; SLIDE {n}")?;
        writeln!(self.writer, "	mov rdi, [r15]")?;
        writeln!(self.writer, "	add r15, {}", n * 8)?;
        writeln!(self.writer, "	mov [r15], rdi")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_update(&mut self, n: usize) -> Result<()> {
        writeln!(self.writer, "	; UPDATE {n}")?;
        writeln!(self.writer, "	stack_pop rdi")?;
        writeln!(self.writer, "	mov rsi, [r15+{}]", n * 8)?;
        writeln!(self.writer, "	mov qword [rsi], TAG_IND")?;
        writeln!(self.writer, "	mov [rsi+8], rdi")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_mkap(&mut self) -> Result<()> {
        writeln!(self.writer, "	; MKAP")?;
        writeln!(self.writer, "	stack_pop rdi")?;
        writeln!(self.writer, "	stack_pop rsi")?;
        writeln!(self.writer, "	call heap_make_app")?;
        writeln!(self.writer, "	stack_push rax")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_pack(&mut self, tag: usize, arity: usize) -> Result<()> {
        writeln!(self.writer, "	; PACK {tag}, {arity}")?;
        writeln!(self.writer, "	mov rdi, {tag}")?;
        writeln!(self.writer, "	mov rsi, {arity}")?;
        writeln!(self.writer, "	mov rdx, r15")?;
        writeln!(self.writer, "	call heap_make_constr")?;
        writeln!(self.writer, "	mov rdi, {}", arity * 8)?;
        writeln!(self.writer, "	add r15, rdi")?;
        writeln!(self.writer, "	stack_push rax")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_split(&mut self, n: usize) -> Result<()> {
        writeln!(self.writer, "	; SPLIT {n}")?;
        writeln!(self.writer, "	stack_pop rsi")?;
        writeln!(self.writer, "	add rsi, 24")?;
        writeln!(self.writer, "	mov rcx, {n}")?;
        writeln!(self.writer, "	mov rdx, {}", n * 8)?;
        writeln!(self.writer, "	sub r15, rdx")?;
        writeln!(self.writer, "	mov rdi, r15")?;
        writeln!(self.writer, "	cld")?;
        writeln!(self.writer, "	rep movsq")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_case(&mut self, branches: &FxHashMap<usize, Vec<Instruction>>) -> Result<()> {
        let case_label = format!(".case.{}", self.case_counter);
        self.case_counter += 1;

        writeln!(self.writer, "	; CASE")?;
        writeln!(self.writer, "	mov rdi, [r15]")?;
        writeln!(self.writer, "	mov rdi, [rdi+8]")?;
        writeln!(self.writer, "	jmp [{case_label}+((rdi-1)*8)]")?;

        writeln!(self.writer, "	{case_label}:")?;
        for (tag, _) in branches {
            writeln!(self.writer, "	dq {case_label}.{tag}")?;
        }
        writeln!(self.writer)?;

        for (tag, insts) in branches {
            writeln!(self.writer, "	{case_label}.{tag}:")?;
            for inst in insts {
                self.emit_instruction(inst)?
            }
            writeln!(self.writer, "	jmp {case_label}.end")?;
        }
        writeln!(self.writer, "	{case_label}.end:")?;

        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_unwind(&mut self) -> Result<()> {
        writeln!(self.writer, "	; UNWIND")?;
        writeln!(self.writer, "	jmp unwind")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_eval(&mut self) -> Result<()> {
        let eval_label = format!(".eval.{}", self.eval_counter);
        self.eval_counter += 1;

        writeln!(self.writer, "	; EVAL")?;
        writeln!(self.writer, "	mov rdi, {eval_label}")?;
        writeln!(self.writer, "	mov rsi, r15")?;
        writeln!(self.writer, "	add rsi, 8")?;
        writeln!(self.writer, "	dump_push rdi, rsi")?;
        writeln!(self.writer, "	jmp unwind")?;
        writeln!(self.writer, "	{eval_label}:")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_instruction(&mut self, inst: &Instruction) -> Result<()> {
        match inst {
            Instruction::PushInt(i) => self.emit_push_int(*i),
            Instruction::PushGlobal(name) => {
                self.emit_push_global(name.as_str(), self.symbols.get(name).unwrap().0)
            }
            Instruction::Alloc(_) => self.emit_alloc(),
            Instruction::Push(n) => self.emit_push(*n),
            Instruction::Pop(n) => self.emit_pop(*n),
            Instruction::Slide(n) => self.emit_slide(*n),
            Instruction::Update(n) => self.emit_update(*n),
            Instruction::MkAp => self.emit_mkap(),
            Instruction::Pack(tag, arity) => self.emit_pack(*tag, *arity),
            Instruction::Split(n) => self.emit_split(*n),
            Instruction::Case(branches) => self.emit_case(&branches),
            Instruction::Eval => self.emit_eval(),
            Instruction::Unwind => self.emit_unwind(),
            Instruction::Print => todo!(),
        }
    }

    fn emit(mut self) -> Result<()> {
        writeln!(self.writer, "{RUNTIME}")?;
        for (symbol, (_, insts)) in self.symbols {
            writeln!(self.writer, "{symbol}:")?;
            for inst in insts {
                self.emit_instruction(inst)?
            }
        }
        self.writer.flush()?;

        Ok(())
    }
}

pub fn emit<'a, 'b, W: Write>(
    writer: &'a mut W,
    symbols: &'b FxHashMap<String, (usize, Vec<Instruction>)>,
) -> Result<()> {
    X86_64::new(writer, symbols).emit()
}
