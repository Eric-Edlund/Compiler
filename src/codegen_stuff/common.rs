use std::collections::HashMap;


#[derive(Clone, Debug, Eq, PartialEq)]
pub enum X86Arg {
    Reg{name: String},
    Immed{value: String},
    // Pseudo assembly
    Var{name: String},
}

#[derive(Debug)]
pub enum X86Instr {
    Movq{src: X86Arg, rd: X86Arg},
    Addq{val: X86Arg, rd: X86Arg},
    Callq{label: String}
}

#[derive(Debug)]
pub struct X86Program {
    pub blocks: HashMap<String, Vec<X86Instr>>,
}
