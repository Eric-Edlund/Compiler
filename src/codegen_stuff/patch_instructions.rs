use crate::codegen_stuff::common::{X86Arg, X86Program};

use super::common::X86Instr;

pub fn patch_instructions(program: &mut X86Program) {
    for (label, block) in &mut program.blocks {
        patch_block(block)
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
        Cmpq {
            a: a_in,
            b: Immed(i),
        } => {
            push(Movq {
                src: Immed(i),
                rd: Reg("rax".to_string()),
            });
            push(Cmpq {
                a: a_in.clone(),
                b: Reg("rax".to_string()),
            })
        }
        // Cmpq only accepts one memory location
        Cmpq {
            a: Deref(r1, offset1),
            b: Deref(r2, offset2),
        } => {
            push(Movq {
                src: Deref(r1, offset1),
                rd: Reg("rax".to_string()),
            });
            push(Cmpq {
                a: Reg("rax".to_string()),
                b: Deref(r2, offset2),
            });
        }

        // Movq only accepts one memory location
        Movq {
            src: Deref(r1, offset1),
            rd: Deref(r2, offset2),
        } => {
            push(Movq {
                src: Deref(r1, offset1),
                rd: Reg("rax".to_string()),
            });
            push(Movq {
                src: Reg("rax".to_string()),
                rd: Deref(r2, offset2),
            });
        }
        Orq(
            Deref(r1, offset1),
            Deref(r2, offset2),
        ) => {
            push(Movq {
                src: Deref(r1, offset1),
                rd: Reg("rax".to_string()),
            });
            push(Orq (
                Reg("rax".to_string()),
                Deref(r2, offset2),
            ));
        }
        Addq {
            val: Deref(r1, offset1),
            rd: Deref(r2, offset2),
        } => {
            push(Movq {
                src: Deref(r1, offset1),
                rd: Reg("rax".to_string()),
            });
            push(Addq {
                val: Reg("rax".to_string()),
                rd: Deref(r2, offset2),
            });
        }
        // X86 sete expects an 8 bit register. We need to find the 8 bit version
        // of the referenced register.
        Sete(Reg(dest)) => {
            push(Sete(Reg(lower_8bits(dest).to_string())));
        }
        Setg(Reg(dest)) => {
            push(Setg(Reg(lower_8bits(dest).to_string())));
        }
        Setl(Reg(dest)) => {
            push(Setl(Reg(lower_8bits(dest).to_string())));
        }
        Setle(Reg(dest)) => {
            push(Setle(Reg(lower_8bits(dest).to_string())));
        }
        Setge(Reg(dest)) => {
            push(Setge(Reg(lower_8bits(dest).to_string())));
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
        "r8" => "r8b",  // intel uses r8b, and amd uses r8l (I'm reading)
        "r9" => "r9b",
        "r10" => "r10b",
        "r11" => "r11b",
        "r12" => "r12b",
        "r13" => "r13b",
        "r14" => "r14b",
        _ => panic!("Made up register name: {}", reg.as_ref()),
    }
}
