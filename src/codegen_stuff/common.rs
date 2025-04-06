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
    Je(String),
    Jmp(String),
    Sete(X86Arg),
    Setl(X86Arg),
    Setle(X86Arg),
    Setg(X86Arg),
    Setge(X86Arg),
    Setne(X86Arg),
    Andq(X86Arg, X86Arg),
    Orq(X86Arg, X86Arg),
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
            Je(to) => {
            }
            Jmp(to) => {
            }
            Setl(rd) => {
                transform(rd);
            }
            Setle(rd) => {
                transform(rd);
            }
            Setg(rd) => {
                transform(rd);
            }
            Setge(rd) => {
                transform(rd);
            }
            Setne(rd) => {
                transform(rd);
            }
            Andq(a, rd) => {
                transform(a);
                transform(rd);
            }
            Orq(a, rd) => {
                transform(a);
                transform(rd);
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
