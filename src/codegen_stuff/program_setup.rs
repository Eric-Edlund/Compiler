use super::x86::{X86Arg, X86Instr, X86Program, CALLEE_SAVED_REGISTERS};
use std::iter::empty;
use X86Arg::*;
use X86Instr::*;

/// Add stack frame setup and teardown to every function.
pub fn wrap_functions_with_stack_logic(program: &mut X86Program) {
    for (name, func) in &mut program.functions {
        let add_frame_name = format!("{}_stack_setup", name);
        func.prefix_block_mut((
            add_frame_name,
            empty()
                // Allocate stack frame
                .chain([
                    // A single instruction which stores %rbp in a spot on the stack,
                    // decrementing rsp enough to make space.
                    Pushq(Reg("rbp")),
                    // Update the base pointer to the current stack pointer
                    Movq(Reg("rsp"), Reg("rbp")),
                    // Mark stack space for the function
                    Subq(Imm(func.stack_size as u64), Reg("rsp")),
                ])
                // Allocate root stack frame, zeroing spaces for the gc
                .chain((0..func.root_stack_size).step_by(8).flat_map(|offset| {
                    [
                        Movq(Imm(0), Deref("r15", 0)),
                        Addq(Imm(8), Deref("r15", 0)),
                    ]
                }))
                // Save callee saved regs
                .chain(
                    CALLEE_SAVED_REGISTERS
                        .iter()
                        .map(|reg| X86Instr::Pushq(X86Arg::Reg(reg))),
                )
                .collect(),
        ));

        let teardown_frame_name = format!("{}_stack_teardown", name);
        func.suffix_block_mut((
            teardown_frame_name,
            empty()
                // Restore callee saved regs
                .chain(
                    CALLEE_SAVED_REGISTERS
                        .iter()
                        .map(|reg| X86Instr::Popq(X86Arg::Reg(reg))),
                )
                // Teardown root stack frame
                .chain([Addq(Imm(func.root_stack_size as u64), Reg("r15"))])
                // Teardown stack frame
                .chain([
                    Addq(Imm(func.stack_size as u64), Reg("rsp")),
                    // rsp -= 8, read it into rbp
                    Popq(Reg("rbp")),
                    // rsp += 8, read that value into %rsp, jump back to previous frame
                    Retq,
                ])
                .collect(),
        ));
    }
}

/// This function creates the main block. The program must not already contain one.
pub fn prelude_and_conclusion(program: &mut X86Program) {
    let entry_fn = program.functions.get_mut(&program.entry_fn).unwrap();

    entry_fn.prefix_block_mut((
        "main".to_string(),
        vec![
            // TODO: How large should the root stack be?
            Movq(Imm(4096), Reg("rdi")),
            // TODO: How large should the heap be?
            Movq(Imm(4049), Reg("rsi")),
            Callq("initialize".to_string()),
            Movq(GlobalVal("rootstack_begin".to_string()), Reg("r15")),
        ],
    ));
}
