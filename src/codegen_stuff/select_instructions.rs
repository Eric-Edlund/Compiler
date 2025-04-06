use crate::codegen_stuff::common::X86Arg;
use crate::parsing::parsing::BasedAstNode;
use crate::parsing::parsing::BinOperation;
use crate::parsing::{parsing::AstNode, FileAnal};
use ordered_hash_map::ordered_map::OrderedHashMap;
use std::sync::Mutex;

use super::common::{X86Instr, X86Program};

static LAST_VAR_NUMBER: Mutex<u32> = Mutex::new(0);
fn new_var_name() -> String {
    let mut curr = *LAST_VAR_NUMBER.lock().unwrap();
    curr += 1;
    return format!("si_tmp_{}", curr);
}

pub fn select_instructions(file: &FileAnal) -> X86Program {
    let mut result = X86Program {
        blocks: OrderedHashMap::new(),
        required_stack_size: 1024,
    };

    result.blocks.insert("start".to_string(), Vec::new());
    let mut current_block = "start".to_string();
    for construct in &file.asts {
        si_stmt(construct, &mut current_block, &mut result.blocks);
    }

    result
}

fn si_stmt(
    exp: &BasedAstNode,
    current_block: &mut String,
    blocks: &mut OrderedHashMap<String, Vec<X86Instr>>,
) {
    use AstNode::*;
    match exp.as_ref() {
        Declaration { identifier, rhs } => {
            let (mut prefix, arg) = si_expr(rhs);
            prefix.push(X86Instr::Movq {
                src: arg,
                rd: X86Arg::Var(identifier.to_string()),
            });
            blocks.get_mut(current_block).unwrap().extend(prefix);
        }
        Assignment { lhs, rhs } => {
            let (mut prepare_value, arg) = si_expr(rhs);
            let Variable { identifier } = lhs.as_ref() else {
                dbg!(lhs);
                panic!("Assignments only should assign to variables.");
            };

            // return [x86.Movq(si_expr(atm1), x86.Reg('rax')),
            //         x86.Addq(si_expr(atm2), x86.Reg('rax')),
            //         x86.Movq(x86.Reg('rax'), x86.Var(x))]
            prepare_value.extend([X86Instr::Movq {
                src: arg,
                rd: X86Arg::Var(identifier.clone()),
            }]);
            blocks.get_mut(current_block).unwrap().extend(prepare_value);
        }
        FunctionDecl { identifier, body } => {
            assert_eq!(identifier, &"main");
            // TODO: Labels for different functions
            si_stmt(body, current_block, blocks);
            blocks
                .get_mut(current_block)
                .unwrap()
                .push(X86Instr::Jmp("conclusion".to_string()));
        }
        FunctionCall { function, ref args } => {
            let Variable { identifier } = function.as_ref() else {
                panic!("Non identifier functions not implemented. (Or allowed?)");
            };
            assert_eq!(identifier, "print", "Only print function is implemented.");

            let (prefix, args) = si_expr(args);

            blocks.get_mut(current_block).unwrap().extend([
                X86Instr::Movq {
                    src: args,
                    rd: X86Arg::Reg("rdi".to_string()),
                },
                X86Instr::Callq {
                    label: "print_int".to_string(),
                },
            ])
        }
        Block { stmts } => {
            for stmt in stmts {
                si_stmt(stmt, current_block, blocks);
            }
        }
        IfStmt {
            condition,
            then_blk,
            else_blk,
        } => {
            let (condition_instrs, condition) = si_expr(condition);
            let then_label = format!("LThen");
            let else_label = format!("LElse");
            let cont_label = format!("Continuation");
            blocks
                .get_mut(current_block)
                .unwrap()
                .extend(condition_instrs);
            blocks.get_mut(current_block).unwrap().extend([
                X86Instr::Cmpq {
                    a: condition,
                    b: X86Arg::Immed { val: 0 },
                },
                X86Instr::Je(else_label.clone()),
                X86Instr::Jmp(then_label.clone()),
            ]);

            *current_block = then_label.clone();
            blocks.insert(current_block.clone(), Vec::new());
            si_stmt(then_blk, current_block, blocks);
            blocks
                .get_mut(current_block)
                .unwrap()
                .push(X86Instr::Jmp(cont_label.clone()));

            if let Some(else_blk) = else_blk {
                *current_block = else_label.clone();
                blocks.insert(current_block.clone(), Vec::new());
                si_stmt(else_blk, current_block, blocks);
                blocks
                    .get_mut(current_block)
                    .unwrap()
                    .push(X86Instr::Jmp(cont_label.clone()));
            }

            *current_block = cont_label.clone();
            blocks.insert(current_block.clone(), Vec::new());
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
        LiteralBool(val) => (vec![], X86Arg::Immed { val: *val as u64 }),
        Variable { identifier } => (vec![], X86Arg::Var(identifier.clone())),
        BinOp {
            op: BinOperation::Add,
            lhs,
            rhs,
        } => {
            let tmp = X86Arg::Var(new_var_name());
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
            lhs,
            rhs,
        } => {
            let tmp = X86Arg::Var(new_var_name());
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
            op: BinOperation::Mult,
            lhs,
            rhs,
        } => {
            let tmp = X86Arg::Var(new_var_name());
            let (prefix, res) = si_expr(lhs);
            let (prefix2, res2) = si_expr(rhs);
            let mut instrs = vec![];
            instrs.extend(prefix);
            instrs.push(X86Instr::Movq {
                src: res,
                rd: tmp.clone(),
            });
            instrs.extend(prefix2);
            instrs.push(X86Instr::Mulq {
                val: res2,
                rd: tmp.clone(),
            });
            (instrs, tmp)
        }
        BinOp { op, lhs, rhs } => {
            let (prefix, res1) = si_expr(lhs);
            let (prefix2, res2) = si_expr(rhs);
            let mut instrs = vec![];
            instrs.extend(prefix);
            instrs.extend(prefix2);
            instrs.push(X86Instr::Cmpq { a: res2.clone(), b: res1.clone() });
            use BinOperation::*;
            use X86Instr::*;
            let tmp = X86Arg::Var(new_var_name());

            instrs.extend(match op {
                Eq => vec![Sete(tmp.clone())],
                Lt => vec![Setl(tmp.clone())],
                LEq => vec![Setle(tmp.clone())],
                Gt => vec![Setg(tmp.clone())],
                GEq => vec![Setge(tmp.clone())],
                NEq => vec![Setne(tmp.clone())],
                And => vec![
                    Movq{src: res1.clone(), rd: tmp.clone()},
                    Andq(res2.clone(), tmp.clone()),
                ],
                Or => vec![
                    Movq{src: res1.clone(), rd: tmp.clone()},
                    Orq(res2.clone(), tmp.clone()),
                ],
                _ => todo!("{:?}", op),
            });
            (instrs, tmp)
        }
        _ => todo!("Unimplemented si_expr {:?}", *exp.as_ref()),
    }
}
