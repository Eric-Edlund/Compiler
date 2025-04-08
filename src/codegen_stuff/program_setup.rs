use super::common::{X86Arg, X86Instr, X86Program};
use X86Instr::*;
use X86Arg::*;


/// Add stack frame setup and teardown to every function.
pub fn wrap_functions_with_stack_logic(program: &mut X86Program) {
    for (name, func) in &mut program.functions {
        let add_frame_name = format!("{}_stack_setup", name);
        let add_frame_blk = vec![
            // A single instruction which stores %rbp in a spot on the stack,
            // decrementing rsp enough to make space.
            Pushq(Reg("rbp")),
            // Update the base pointer to the current stack pointer
            Movq(Reg("rsp"), Reg("rbp")),
            Subq(Imm(func.stack_size as u64), Reg("rsp")),
            Jmp(func.lead_block.clone()),
        ];
        func.blocks.insert(add_frame_name.clone(), add_frame_blk);
        func.lead_block = add_frame_name.clone();

        func.blocks.insert(format!("{}_stack_teardown", name), vec![
            Addq(Imm(func.stack_size as u64), Reg("rsp")),
            // rsp -= 8, read it into rbp
            Popq(Reg("rbp")),
            // rsp += 8, read that value into %rsp, jump back (somewhere)??? TODO
            Retq,
        ]);
    }
}

pub fn prelude_and_conclusion(program: &mut X86Program) {
    let entry_fn = program.functions.get_mut(&program.entry_fn).unwrap();

    entry_fn.blocks.insert("main".to_string(), vec![
        // For some reason the registers start with initial junk values.
        Movq(Imm(0), Reg("rsi")),
        Movq(Imm(0), Reg("rdi")),
        Movq(Imm(0), Reg("rax")),
        Movq(Imm(0), Reg("rbx")),
        Movq(Imm(0), Reg("rcx")),
        Movq(Imm(0), Reg("rdx")),
        Movq(Imm(0), Reg("r8")),
        Movq(Imm(0), Reg("r9")),
        Movq(Imm(0), Reg("r10")),
        Movq(Imm(0), Reg("r11")),
        Movq(Imm(0), Reg("r12")),
        Movq(Imm(0), Reg("r13")),
        Movq(Imm(0), Reg("r14")),
        Movq(Imm(0), Reg("r15")),
        Jmp(entry_fn.lead_block.clone()),
    ]);
    program.entry_fn = "main".to_string();
}
