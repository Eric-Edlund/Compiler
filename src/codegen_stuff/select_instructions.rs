use crate::codegen_stuff::common::X86Arg;
use crate::parsing::parsing::BasedAstNode;
use crate::parsing::parsing::BinOperation;
use crate::parsing::{parsing::AstNode, FileAnal};
use std::collections::HashMap;
use std::sync::Mutex;

use super::common::{X86Instr, X86Program};

static LAST_VAR_NUMBER: Mutex<u32> = Mutex::new(0);
fn new_var_name() -> String {
    let mut curr = *LAST_VAR_NUMBER.lock().unwrap();
    curr += 1;
    return format!("V{}", curr);
}

pub fn select_instructions(file: &FileAnal) -> X86Program {
    let mut result = X86Program {
        blocks: HashMap::new(),
        required_stack_size: 1024,
    };

    result.blocks.insert("main".to_string(), Vec::new());
    for construct in &file.asts {
        result
            .blocks
            .get_mut("main")
            .unwrap()
            .extend(si_stmt(construct));
    }

    result
}

fn si_stmt(exp: &BasedAstNode) -> Vec<X86Instr> {
    use AstNode::*;
    match exp.as_ref() {
        Declaration { identifier, rhs } => {
            let (mut prefix, arg) = si_expr(rhs);
            prefix.push(X86Instr::Movq {
                src: arg,
                rd: X86Arg::Var {
                    name: identifier.to_string(),
                },
            });
            prefix
        }
        Assignment { lhs, rhs } => {
            let (mut prepare_value, arg) = si_expr(rhs);
            let Variable { identifier } = lhs.as_ref() else {
                panic!("Assignments only should assign to variables.");
            };
            prepare_value.push(X86Instr::Movq {
                src: arg,
                rd: X86Arg::Var {
                    name: identifier.clone(),
                },
            });
            prepare_value
        }
        FunctionDecl { identifier, body } => {
            assert_eq!(identifier, &"main");

            si_stmt(body)
        }
        FunctionCall { function, ref args } => {
            let Variable { identifier } = function.as_ref() else {
                panic!("Non identifier functions not implemented. (Or allowed?)");
            };
            assert_eq!(identifier, "print", "Only print function is implemented.");

            let (prefix, args) = si_expr(args);

            vec![
                X86Instr::Movq {
                    src: args,
                    rd: X86Arg::Reg {
                        name: "rdi".to_string(),
                    },
                },
                X86Instr::Callq {
                    label: "print_int".to_string(),
                },
            ]
        }
        Block { stmts } => {
            let mut instrs = Vec::<X86Instr>::new();
            for stmt in stmts {
                instrs.extend(si_stmt(stmt));
            }
            instrs
        }
        IfStmt {condition, then_blk, else_blk} => {
            let mut instrs = Vec::<X86Instr>::new();
            let (is, _) = si_expr(condition);
            let l_else = X86Arg::Label("LElse".to_string());
            let l_after = X86Arg::Label("LAfter".to_string());
            instrs.extend(is);
            instrs.push(X86Instr::Je{to: l_else});
            let is = si_stmt(then_blk);
            instrs.extend(is);
            instrs.push(X86Instr::Label("LElse".to_string()));
            let is = si_stmt(then_blk);
            instrs.extend(is);
            instrs.push(X86Instr::Label("LAfter".to_string()));
            instrs
            
        }
        x => panic!("Si_statment not implemented for {:?}", x),
    }
}

// Accepts expression nodes. It is assumed that after the rco pass,
// an expression has at most depth = 1, for binary operations.
// Binary operations do not have child binary operations.
fn si_expr(exp: &BasedAstNode) -> (Vec<X86Instr>, X86Arg) {
    use AstNode::*;
    match exp.as_ref() {
        LiteralNumber(val) => (vec![], X86Arg::Immed { val: *val as u64 }),
        Variable { identifier } => (
            vec![],
            X86Arg::Var {
                name: identifier.clone(),
            },
        ),
        BinOp {
            op: BinOperation::Add,
            ref lhs,
            ref rhs,
        } => {
            let tmp = X86Arg::Var {
                name: new_var_name(),
            };
            let (prefix, res) = si_expr(lhs);
            let (prefix2, res2) = si_expr(rhs);
            let mut instrs = vec![];
            instrs.extend(prefix);
            instrs.push(X86Instr::Movq {
                src: res,
                rd: tmp.clone(),
            });
            instrs.extend(prefix2);
            instrs.push(X86Instr::Addq {
                val: res2,
                rd: tmp.clone(),
            });
            (instrs, tmp)
        }
        BinOp {
            op: BinOperation::Sub,
            ref lhs,
            ref rhs,
        } => {
            let tmp = X86Arg::Var {
                name: new_var_name(),
            };
            let (prefix, res) = si_expr(lhs);
            let (prefix2, res2) = si_expr(rhs);
            let mut instrs = vec![];
            instrs.extend(prefix);
            instrs.push(X86Instr::Movq {
                src: res,
                rd: tmp.clone(),
            });
            instrs.extend(prefix2);
            instrs.push(X86Instr::Subq {
                val: res2,
                rd: tmp.clone(),
            });
            (instrs, tmp)
        }
        BinOp {
            op: BinOperation::Eq,
            ref lhs,
            ref rhs,
        } => {
            let tmp = X86Arg::Var {
                name: new_var_name(),
            };
            let (prefix, res) = si_expr(lhs);
            let (prefix2, res2) = si_expr(rhs);
            let mut instrs = vec![];
            instrs.extend(prefix2);
            instrs.push(X86Instr::Cmpq {
                a: res2,
                b: tmp.clone(),
            });
            (instrs, tmp)
        }
        _ => todo!("Unimplemented si_expr {:?}", *exp.as_ref()),
    }
}
