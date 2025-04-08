use crate::codegen_stuff::common::{X86Arg, X86Program};

use super::common::X86Instr;

pub fn patch_instructions(program: &mut X86Program) {
    for (name, function) in &mut program.functions {
        for (label, block) in &mut function.blocks {
            patch_block(block)
        }
    }
}

/// Replaces the block with a patched version.
fn patch_block(block: &mut Vec<X86Instr>) {
    let mut new_instrs = vec![];

    for instr in block.drain(0..) {
        patch_instr(instr, |i| {
            new_instrs.push(i);
        });
    }

    *block = new_instrs;
}

fn patch_instr<F>(instr: X86Instr, mut push: F)
where
    F: FnMut(X86Instr),
{
    use X86Arg::*;
    use X86Instr::*;
    match instr {
        Cmpq(a_in, Imm(i)) => {
            push(Movq(Imm(i), Reg("rax")));
            push(Cmpq(a_in.clone(), Reg("rax")));
        }
        // Cmpq only accepts one memory location
        Cmpq(Deref(r1, offset1), Deref(r2, offset2)) => {
            push(Movq(Deref(r1, offset1), Reg("rax")));
            push(Cmpq(Reg("rax"), Deref(r2, offset2)));
        }

        // Movq only accepts one memory location
        Movq(Deref(r1, offset1), Deref(r2, offset2)) => {
            push(Movq(Deref(r1, offset1), Reg("rax")));
            push(Movq(Reg("rax"), Deref(r2, offset2)));
        }
        Orq(Deref(r1, offset1), Deref(r2, offset2)) => {
            push(Movq(Deref(r1, offset1), Reg("rax")));
            push(Orq(Reg("rax"), Deref(r2, offset2)));
        }
        Addq(Deref(r1, offset1), Deref(r2, offset2)) => {
            push(Movq(Deref(r1, offset1), Reg("rax")));
            push(Addq(Reg("rax"), Deref(r2, offset2)));
        }
        // X86 sete expects an 8 bit register. We need to find the 8 bit version
        // of the referenced register.
        Sete(Reg(dest)) => {
            push(Sete(Reg(lower_8bits(dest))));
        }
        Setg(Reg(dest)) => {
            push(Setg(Reg(lower_8bits(dest))));
        }
        Setl(Reg(dest)) => {
            push(Setl(Reg(lower_8bits(dest))));
        }
        Setle(Reg(dest)) => {
            push(Setle(Reg(lower_8bits(dest))));
        }
        Setge(Reg(dest)) => {
            push(Setge(Reg(lower_8bits(dest))));
        }
        // Movzbq can't move into a deref
        Movzbq(Reg(src), Deref(reg, offset)) => {
            push(Movzbq(Reg(lower_8bits(src)), Reg("rax")));
            push(Movq(Reg("rax"), Deref(reg, offset)));
        }
        _ => push(instr),
    }
}

fn lower_8bits<R>(reg: R) -> &'static str
where
    R: AsRef<str>,
{
    match reg.as_ref() {
        "rax" => "al",
        "rbx" => "bl",
        "rcx" => "cl",
        "rdx" => "dl",
        "rsi" => "sil",
        "r8" => "r8b", // intel uses r8b, and amd uses r8l (I'm reading)
        "r9" => "r9b",
        "r10" => "r10b",
        "r11" => "r11b",
        "r12" => "r12b",
        "r13" => "r13b",
        "r14" => "r14b",
        "al" => "al",
        "bl" => "bl",
        "cl" => "cl",
        "dl" => "dl",
        "r8b" => "r8b",
        "r9b" => "r9b",
        "r10b" => "r10b",
        "r11b" => "r11b",
        "r12b" => "r12b",
        "r13b" => "r13b",
        "r14b" => "r14b",
        _ => panic!("Made up register name: {}", reg.as_ref()),
    }
}
