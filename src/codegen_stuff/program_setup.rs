use super::common::{X86Arg, X86Instr, X86Program};
use X86Instr::*;
use X86Arg::*;


pub fn prelude_and_conclusion(program: &mut X86Program) {
    let main = program.blocks.get_mut("main").unwrap();
    main.insert(0, Pushq{ rd: Reg{name: "rbp".to_string()}});
    main.insert(0, Movq{src: Reg{name: "rsp".to_string()}, rd: Reg{name: "rbp".to_string()}});
    main.insert(0, Subq{val: Immed{val: program.required_stack_size as u64}, rd: Reg{name: "rsp".to_string()}});

    main.extend(vec![
        Addq{val: Immed{val: program.required_stack_size as u64}, rd: Reg{name: "rsp".to_string()}},
        Popq{rd: Reg{name: "rbp".to_string()}},
        Retq,
    ])
}
