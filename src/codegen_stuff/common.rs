use std::collections::HashMap;

use ordered_hash_map::OrderedHashMap;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum X86Arg {
    Reg(&'static str),
    Imm(u64),
    Label(String),
    Deref(&'static str, i32),
    // Pseudo assembly
    Var(String),
}

#[derive(Debug)]
pub enum X86Instr {
    /// Destination register is always last
    Movq(X86Arg, X86Arg),
    Movzbq(X86Arg, X86Arg),
    Addq(X86Arg, X86Arg),
    Subq(X86Arg, X86Arg),
    Imulq { val: X86Arg, b: X86Arg, rd: X86Arg },
    Callq(String),
    Pushq(X86Arg),
    Popq(X86Arg),
    Cmpq { a: X86Arg, b: X86Arg },
    Retq,
    Je(String),
    Jne(String),
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
            Retq | Callq { .. } => {}
            Movq(src, rd) => {
                transform(src);
                transform(rd);
            }
            Movzbq(from, to) => {
                transform(from);
                transform(to);
            }
            Addq(val, rd) => {
                transform(val);
                transform(rd);
            }
            Subq (val, rd ) => {
                transform(val);
                transform(rd);
            }
            Imulq { val, b, rd } => {
                transform(val);
                transform(b);
                transform(rd);
            }
            Cmpq { a, b } => {
                transform(a);
                transform(b);
            }
            Sete(rd) => {
                transform(rd);
            }
            Je(to) => {}
            Jne(to) => {}
            Jmp(to) => {}
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
            x => todo!("{:?}", x),
        }
    }
}

#[derive(Debug)]
pub struct X86Function {
    pub name: String,
    pub lead_block: String,
    pub tail_block: String,
    pub blocks: OrderedHashMap<String, Vec<X86Instr>>,
    pub stack_size: u32,
}

impl X86Function {
    /// Adds the block to the program in place of the lead_block,
    /// mutates the added block to jump to the previous lead block.
    pub fn prefix_block_mut(&mut self, mut block: (String, Vec<X86Instr>)) {
        block.1.push(X86Instr::Jmp(self.lead_block.clone()));
        self.lead_block = block.0.clone();
        self.blocks.insert(block.0, block.1);
    }

    /// Adds the block to the end of the program replacing the tail block.
    /// Mutates the existing tail to jump to the added block.
    pub fn suffix_block_mut(&mut self, block: (String, Vec<X86Instr>)) {
        self.blocks.get_mut(&self.tail_block).unwrap().extend([
            X86Instr::Jmp(block.0.clone()),
        ]);
        self.tail_block = block.0.clone();
        self.blocks.insert(block.0, block.1);

    }
}

#[derive(Debug)]
pub struct X86Program {
    pub functions: HashMap<String, X86Function>,
    pub entry_fn: String,
}
