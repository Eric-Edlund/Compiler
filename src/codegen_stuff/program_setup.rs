use super::common::{X86Arg, X86Instr, X86Program};
use X86Instr::*;
use X86Arg::*;


pub fn prelude_and_conclusion(program: &mut X86Program) {
    program.blocks.insert("main".to_string(), vec![
        Subq{val: Immed{val: program.required_stack_size as u64}, rd: Reg("rsp".to_string())},
        Movq{src: Reg("rsp".to_string()), rd: Reg("rbp".to_string())},
        Pushq{ rd: Reg("rbp".to_string())},
        Jmp("start".to_string()),
    ]);

    program.blocks.insert("conclusion".to_string(), vec![
        Addq{val: Immed{val: program.required_stack_size as u64}, rd: Reg("rsp".to_string())},
        Popq{rd: Reg("rbp".to_string())},
        Retq,
    ]);
}
