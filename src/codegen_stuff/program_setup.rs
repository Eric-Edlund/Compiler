use super::common::{X86Arg, X86Instr, X86Program};
use X86Instr::*;
use X86Arg::*;


pub fn prelude_and_conclusion(program: &mut X86Program) {
    program.blocks.insert("main".to_string(), vec![
        // For some reason, on my machine the registers start with initial junk values.
        Movq{src: Immed(0), rd: Reg("rsi".to_string())},
        Movq{src: Immed(0), rd: Reg("rdi".to_string())},
        Movq{src: Immed(0), rd: Reg("rax".to_string())},
        Movq{src: Immed(0), rd: Reg("rbx".to_string())},
        Movq{src: Immed(0), rd: Reg("rcx".to_string())},
        Movq{src: Immed(0), rd: Reg("rdx".to_string())},
        Movq{src: Immed(0), rd: Reg("r8".to_string())},
        Movq{src: Immed(0), rd: Reg("r9".to_string())},
        Movq{src: Immed(0), rd: Reg("r10".to_string())},
        Movq{src: Immed(0), rd: Reg("r11".to_string())},
        Movq{src: Immed(0), rd: Reg("r12".to_string())},
        Movq{src: Immed(0), rd: Reg("r13".to_string())},
        Movq{src: Immed(0), rd: Reg("r14".to_string())},

        // Stack setup
        Subq{val: Immed(program.required_stack_size as u64), rd: Reg("rsp".to_string())},
        Movq{src: Reg("rsp".to_string()), rd: Reg("rbp".to_string())},
        Pushq{ rd: Reg("rbp".to_string())},
        Jmp("start".to_string()),
    ]);

    program.blocks.insert("conclusion".to_string(), vec![
        Addq{val: Immed(program.required_stack_size as u64), rd: Reg("rsp".to_string())},
        Popq{rd: Reg("rbp".to_string())},
        Retq,
    ]);
}
