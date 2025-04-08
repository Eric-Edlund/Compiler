use super::common::{X86Arg, X86Instr, X86Program};

use X86Instr::*;

pub fn render(program: &X86Program) -> Vec<u8> {
    let mut res = Vec::<u8>::new();

    let entry_fn = &program.functions[&program.entry_fn];
    res.extend(format!("  .globl {}\n", entry_fn.lead_block).bytes());

    for (name, function) in &program.functions {
        for (label, instrs) in &function.blocks {
            res.extend(format!("{}:\n", label).bytes());

            for instr in instrs {
                render_instr(instr, &mut res)
            }
        }
    }
    res
}

fn render_instr(instr: &X86Instr, res: &mut Vec<u8>) {
    let bytes = match instr {
        Addq(val, rd) => format!("  addq {}, {}\n", render_expr(val), render_expr(rd)),
        Subq(val, rd) => format!("  subq {}, {}\n", render_expr(val), render_expr(rd)),
        Imulq(a, b, rd) => format!(
            "  imulq {}, {}, {}\n",
            render_expr(a),
            render_expr(b),
            render_expr(rd)
        ),
        Movq(src, rd) => format!("  movq {}, {}\n", render_expr(src), render_expr(rd)),
        Movzbq(src, rd) => format!("  movzbq {}, {}\n", render_expr(src), render_expr(rd)),
        Callq(label) => format!("  callq {}\n", label),
        Pushq(rd) => format!("  pushq {}\n", render_expr(rd)),
        Popq(rd) => format!("  popq {}\n", render_expr(rd)),
        Cmpq(a, b) => format!("  cmpq {}, {}\n", render_expr(a), render_expr(b)),
        Retq => "  retq\n".to_string(),
        Je(to) => format!("  je {}\n", to),
        Jne(to) => format!("  jne {}\n", to),
        Jmp(to) => format!("  jmp {}\n", to),
        Sete(rd) => format!("  sete {}\n", render_expr(rd)),
        Setl(rd) => format!("  setl {}\n", render_expr(rd)),
        Setle(rd) => format!("  setle {}\n", render_expr(rd)),
        Setg(rd) => format!("  setg {}\n", render_expr(rd)),
        Setge(rd) => format!("  setge {}\n", render_expr(rd)),
        Setne(rd) => format!("  setne {}\n", render_expr(rd)),
        Andq(a, rd) => format!("  andq {}, {}\n", render_expr(a), render_expr(rd)),
        Orq(a, rd) => format!("  orq {}, {}\n", render_expr(a), render_expr(rd)),
    };
    res.extend(bytes.into_bytes());
}

fn render_expr(arg: &X86Arg) -> String {
    match arg {
        X86Arg::Reg(name) => format!("%{}", name),
        X86Arg::Var(name) => name.to_string(),
        X86Arg::Imm(value) => format!("${}", value),
        X86Arg::Label(name) => name.to_string(),
        X86Arg::Deref(reg, offset) => format!("{}(%{})", offset, reg),
    }
}
