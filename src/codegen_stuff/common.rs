use std::collections::HashMap;


#[derive(Clone, Debug, Eq, PartialEq)]
pub enum X86Arg {
    Reg{name: String},
    Immed{val: u64},
    Label(String),
    // Pseudo assembly
    Var{name: String},
}

#[derive(Debug)]
pub enum X86Instr {
    Label(String),
    Movq{src: X86Arg, rd: X86Arg},
    Addq{val: X86Arg, rd: X86Arg},
    Subq{val: X86Arg, rd: X86Arg},
    Callq{label: String},
    Pushq{rd: X86Arg},
    Popq{rd: X86Arg},
    Cmpq{a: X86Arg, b: X86Arg},
    Retq,
    Je{to: X86Arg},
}

#[derive(Debug)]
pub struct X86Program {
    pub blocks: HashMap<String, Vec<X86Instr>>,
    pub required_stack_size: u32,
}
