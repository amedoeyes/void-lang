use std::io::Write;

use fxhash::FxHashMap;

use crate::{
    codegen::{Backend, Result},
    ir::instructions::Instruction,
};

const RUNTIME: &str = r#"
default rel

section .bss
__stack_base resq 1024
__dump_base resq 2048
__heap_start resq 4096
__heap_free_ptr resq 1

__TAG_INT equ 0
__TAG_CNST equ 1
__TAG_IND equ 2
__TAG_APP equ 3
__TAG_GLBL equ 4

section .text
global  _start

__heap_alloc:
	mov rax, [__heap_free_ptr]
	add [__heap_free_ptr], rdi
	ret

__heap_make_int:
	mov  rsi, rdi
	mov  rdi, 16
	call __heap_alloc
	mov  qword [rax], __TAG_INT
	mov  qword [rax+8], rsi
	ret

__heap_make_ind:
	mov  rsi, rdi
	mov  rdi, 16
	call __heap_alloc
	mov  qword [rax], __TAG_IND
	mov  qword [rax+8], rsi
	ret

__heap_make_app:
	mov  rdx, rdi
	mov  rdi, 24
	call __heap_alloc
	mov  qword [rax], __TAG_APP
	mov  qword [rax+8], rdx
	mov  qword [rax+16], rsi
	ret

__heap_make_global:
	mov  rdx, rdi
	mov  rdi, 24
	call __heap_alloc
	mov  qword [rax], __TAG_GLBL
	mov  qword [rax+8], rdx
	mov  qword [rax+16], rsi
	ret

__heap_make_constr:
	mov rcx, rsi
	mov rsi, rdi

	mov rdi, rcx
	shl rdi, 3
	add rdi, 24
	call __heap_alloc

	mov qword [rax], __TAG_CNST
	mov qword [rax+8], rsi
	mov qword [rax+16], rcx

	lea rdi, [rax+24]
	mov rsi, rdx
	cld
	rep movsq

	ret

__unwind:
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
		mov rsi, [r14]
		mov rdx, [r14+8]
		add r14, 16

		mov r15, rdx
		sub r15, 8
		mov [r15], rdi
		jmp rsi

	.tag_ind:
		mov rdi, [rdi+8]
		mov qword [r15], rdi
		jmp __unwind

	.tag_app:
		mov rdi, [rdi+8]
		sub r15, 8
		mov [r15], rdi
		jmp __unwind

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

__force:
	sub r15, 8
	mov [r15], rdi

	mov rdi, .return
	mov rsi, r15
	add rsi, 8

	sub r14, 16
	mov [r14], rdi
	mov [r14+8], rsi

	jmp __unwind

	.return:

	mov rax, [r15]
	add r15, 8

	ret

__print_int:
	xor r8, r8

	test rdi, rdi
	jns .positive

	mov r8, 1
	neg rdi

	.positive:
	mov rax, rdi

	lea rsi, [rsp]
	mov rcx, 10
	.loop:
		xor rdx, rdx
		div rcx
		add dl, '0'
		dec rsi
		mov [rsi], dl
		test rax, rax
		jnz .loop

	test r8, r8
	jz .done

	dec rsi
	mov [rsi], '-'

	.done:
	mov rdx, rsp
	sub rdx, rsi

	mov rax, 1
	mov rdi, 1
	syscall
	ret

__print_char:
	mov [rsp-1], dil
	mov rax, 1
	mov rdi, 1
	lea rsi, [rsp-1]
	mov rdx, 1
	syscall
	ret

__print_str:
	mov rdx, rsi
	mov rsi, rdi
	mov rax, 1
	mov rdi, 1
	syscall
	ret

__print:
	call __force
	mov r8, rax

	cmp [r8], __TAG_INT
	je .tag_int
	cmp [r8], __TAG_CNST
	je .tag_cnst

	jmp .done

	.tag_int:
		mov rdi, [r8+8]
		call __print_int
		jmp .done

	.tag_cnst:
		mov rdi, '<'
		call __print_char

		mov rdi, [r8+8]
		call __print_int

		mov rdi, '>'
		call __print_char

		mov rdi, [r8+16]
		test rdi, rdi
		jz .done

		mov rdi, '('
		call __print_char

		push r8
		mov rdi, [r8+24]
		call __print
		pop r8

		mov r9, 1
		.loop:
			cmp r9, [r8+16]
			je .loop_end

			mov rdi, ','
			call __print_char

			mov rdi, ' '
			call __print_char

			push r8
			push r9
			mov rdi, [r8+24+r9*8]
			call __print
			pop r9
			pop r8

			inc r9
			jmp .loop

		.loop_end:

		mov rdi, ')'
		call __print_char

		jmp .done

	.done:

	ret

__println:
	call __print
	mov rdi, 10
	call __print_char

__runtime_init:
	mov r15, __stack_base + 1024*8
	mov r14, __dump_base + 2048*8
	mov rdi, __heap_start
	mov [__heap_free_ptr], rdi
	ret

__start_main:
	; PUSHGLOBAL main, 0
	mov rdi, main
	mov rsi, 0
	call __heap_make_global
	sub r15, 8
	mov [r15], rax

	; EVAL
	mov rdi, .return
	mov rsi, r15
	add rsi, 8

	sub r14, 16
	mov [r14], rdi
	mov [r14+8], rsi

	jmp __unwind

	.return:

	mov rdi, [r15]
	add r15, 8

	call __println

	ret

_start:
	call __runtime_init

	call __start_main

	mov rdi, 0
	mov rax, 60
	syscall
"#;

pub struct X86_64<'a, 'b, W: Write> {
    writer: &'a mut W,
    symbols: &'b FxHashMap<String, Vec<Instruction>>,
    case_counter: usize,
    eval_counter: usize,
}

impl<'a, 'b, W: Write> X86_64<'a, 'b, W> {
    pub fn new(writer: &'a mut W, symbols: &'b FxHashMap<String, Vec<Instruction>>) -> Self {
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
        writeln!(self.writer, "	call __heap_make_int")?;
        writeln!(self.writer, "	sub r15, 8")?;
        writeln!(self.writer, "	mov [r15], rax")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_push_global(&mut self, name: &str, arity: usize) -> Result<()> {
        writeln!(self.writer, "	; PUSHGLOBAL {name}")?;
        writeln!(self.writer, "	mov rdi, {name}")?;
        writeln!(self.writer, "	mov rsi, {arity}")?;
        writeln!(self.writer, "	call __heap_make_global")?;
        writeln!(self.writer, "	sub r15, 8")?;
        writeln!(self.writer, "	mov [r15], rax")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_alloc(&mut self) -> Result<()> {
        writeln!(self.writer, "	; ALLOC")?;
        writeln!(self.writer, "	mov rdi, 0")?;
        writeln!(self.writer, "	call __heap_make_ind")?;
        writeln!(self.writer, "	sub r15, 8")?;
        writeln!(self.writer, "	mov [r15], rax")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_push(&mut self, n: usize) -> Result<()> {
        writeln!(self.writer, "	; PUSH {n}")?;
        writeln!(self.writer, "	mov rdi, [r15+{}]", n * 8)?;
        writeln!(self.writer, "	sub r15, 8")?;
        writeln!(self.writer, "	mov [r15], rdi")?;
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
        writeln!(self.writer, "	mov rdi, [r15]")?;
        writeln!(self.writer, "	add r15, 8")?;
        writeln!(self.writer, "	mov rsi, [r15+{}]", n * 8)?;
        writeln!(self.writer, "	mov qword [rsi], __TAG_IND")?;
        writeln!(self.writer, "	mov [rsi+8], rdi")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_mkap(&mut self) -> Result<()> {
        writeln!(self.writer, "	; MKAP")?;
        writeln!(self.writer, "	mov rdi, [r15]")?;
        writeln!(self.writer, "	mov rsi, [r15+8]")?;
        writeln!(self.writer, "	add r15, 16")?;
        writeln!(self.writer, "	call __heap_make_app")?;
        writeln!(self.writer, "	sub r15, 8")?;
        writeln!(self.writer, "	mov [r15], rax")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_pack(&mut self, tag: usize, arity: usize) -> Result<()> {
        writeln!(self.writer, "	; PACK {tag}, {arity}")?;
        writeln!(self.writer, "	mov rdi, {tag}")?;
        writeln!(self.writer, "	mov rsi, {arity}")?;
        writeln!(self.writer, "	mov rdx, r15")?;
        writeln!(self.writer, "	call __heap_make_constr")?;
        writeln!(self.writer, "	mov rdi, {}", arity * 8)?;
        writeln!(self.writer, "	add r15, rdi")?;
        writeln!(self.writer, "	sub r15, 8")?;
        writeln!(self.writer, "	mov [r15], rax")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_unpack(&mut self, n: usize) -> Result<()> {
        writeln!(self.writer, "	; UNPACK {n}")?;
        writeln!(self.writer, "	mov rsi, [r15]")?;
        writeln!(self.writer, "	add r15, 8")?;
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
        writeln!(self.writer, "	; __UNWIND")?;
        writeln!(self.writer, "	jmp __unwind")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_eval(&mut self) -> Result<()> {
        let eval_label = format!(".eval.{}", self.eval_counter);
        self.eval_counter += 1;

        writeln!(self.writer, "	; EVAL")?;
        writeln!(self.writer, "	mov rdi, [r15]")?;
        writeln!(self.writer, "	cmp rdi, __TAG_INT")?;
        writeln!(self.writer, "	je {eval_label}")?;
        writeln!(self.writer, "	cmp rdi, __TAG_CNST")?;
        writeln!(self.writer, "	je {eval_label}")?;
        writeln!(self.writer, "	mov rdi, {eval_label}")?;
        writeln!(self.writer, "	mov rsi, r15")?;
        writeln!(self.writer, "	add rsi, 8")?;
        writeln!(self.writer, "	sub r14, 16")?;
        writeln!(self.writer, "	mov [r14], rdi")?;
        writeln!(self.writer, "	mov [r14+8], rsi")?;
        writeln!(self.writer, "	jmp __unwind")?;
        writeln!(self.writer, "	{eval_label}:")?;
        writeln!(self.writer)?;
        Ok(())
    }

    fn emit_instruction(&mut self, inst: &Instruction) -> Result<()> {
        match inst {
            Instruction::PushInt(i) => self.emit_push_int(*i),
            Instruction::PushGlobal(name, arity) => self.emit_push_global(name.as_str(), *arity),
            Instruction::Alloc => self.emit_alloc(),
            Instruction::Push(n) => self.emit_push(*n),
            Instruction::Pop(n) => self.emit_pop(*n),
            Instruction::Slide(n) => self.emit_slide(*n),
            Instruction::Update(n) => self.emit_update(*n),
            Instruction::MkAp => self.emit_mkap(),
            Instruction::Pack(tag, arity) => self.emit_pack(*tag, *arity),
            Instruction::Unpack(n) => self.emit_unpack(*n),
            Instruction::Case(branches) => self.emit_case(&branches),
            Instruction::Eval => self.emit_eval(),
            Instruction::Unwind => self.emit_unwind(),
        }
    }

    fn emit(mut self) -> Result<()> {
        writeln!(self.writer, "{RUNTIME}")?;

        for (symbol, insts) in self.symbols {
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
    symbols: &'b FxHashMap<String, Vec<Instruction>>,
) -> Result<()> {
    X86_64::new(writer, symbols).emit()
}
