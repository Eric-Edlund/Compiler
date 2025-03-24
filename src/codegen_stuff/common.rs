use std::collections::HashMap;

use ordered_hash_map::OrderedHashMap;


#[derive(Clone, Debug, Eq, PartialEq)]
pub enum X86Arg {
    Reg(String),
    Immed{val: u64},
    Label(String),
    Deref(String, i32),
    // Pseudo assembly
    Var(String),
}

#[derive(Debug)]
pub enum X86Instr {
    Movq{src: X86Arg, rd: X86Arg},
    Addq{val: X86Arg, rd: X86Arg},
    Subq{val: X86Arg, rd: X86Arg},
    Callq{label: String},
    Pushq{rd: X86Arg},
    Popq{rd: X86Arg},
    Cmpq{a: X86Arg, b: X86Arg},
    Retq,
    Je{to: X86Arg},
    Jmp(X86Arg),
    Sete(X86Arg),
}

impl X86Instr {
    pub fn transform_args<T: Fn(&mut X86Arg)>(&mut self, transform: T) {
        use X86Instr::*;
        match self {
            Retq | Callq{..} => {},
            Movq{src, rd} => {
                transform(src);
                transform(rd);
            }
            Addq{val, rd} => {
                transform(val);
                transform(rd);
            }
            Subq{val, rd} => {
                transform(val);
                transform(rd);
            }
            Cmpq{a, b} => {
                transform(a);
                transform(b);
            }
            Sete(rd) => {
                transform(rd);
            }
            Je {to} => {
                transform(to);
            }
            Jmp(to) => {
                transform(to);
            }
            x => todo!("{:?}", x)
        }
    }
}

#[derive(Debug)]
pub struct X86Program {
    pub blocks: OrderedHashMap<String, Vec<X86Instr>>,
    pub required_stack_size: u32,
}
