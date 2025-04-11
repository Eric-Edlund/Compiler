use crate::codegen_stuff::x86::X86Arg;
use crate::codegen_stuff::x86::X86Function;
use crate::parsing::parsing::BasedAstNode;
use crate::parsing::parsing::BinOperation;
use crate::parsing::{parsing::AstNode, FileAnal};
use ordered_hash_map::ordered_map::OrderedHashMap;
use std::collections::HashMap;
use std::sync::Mutex;

use super::x86::{X86Instr, X86Program};

static LAST_VAR_NUMBER: Mutex<u32> = Mutex::new(0);
fn new_var_name() -> String {
    let mut curr = LAST_VAR_NUMBER.lock().unwrap();
    *curr += 1;
    return format!("si_tmp_{}", *curr);
}

static LAST_LABEL_NUM: Mutex<u32> = Mutex::new(0);
fn next_label() -> String {
    let mut curr = LAST_LABEL_NUM.lock().unwrap();
    *curr += 1;
    return format!("L{}", *curr);
}

pub fn select_instructions(file: &FileAnal) -> X86Program {
    let functions: HashMap<String, BasedAstNode> = file
        .asts
        .iter()
        .map(|node| {
            let AstNode::FunctionDecl { identifier, body } = node.as_ref() else {
                panic!("All file top level structures are function declarations.");
            };
            (identifier.to_string(), body.clone())
        })
        .collect();

    assert!(functions.contains_key("main"));

    let mut functions = HashMap::new();

    for construct in &file.asts {
        let f = si_func_decl(construct);
        functions.insert(f.name.clone(), f);
    }

    X86Program {
        functions,
        entry_fn: "main".to_string(),
    }
}

fn si_func_decl(exp: &BasedAstNode) -> X86Function {
    use AstNode::*;
    let FunctionDecl { identifier, body } = exp.as_ref() else {
        panic!("Expecting a function declaration.");
    };
    assert_eq!(identifier, &"main");

    let mut blocks = OrderedHashMap::new();

    let this_lead = format!("{}_start", identifier);
    let this_tail = format!("{}_conclusion", identifier);
    blocks.insert(this_lead.clone(), vec![]);

    let mut curr = this_lead.clone();
    si_stmt(body, &mut curr, &mut blocks);

    blocks.get_mut(&curr).unwrap().extend([
        X86Instr::Jmp(this_tail.clone()),
    ]);

    blocks.insert(this_tail.clone(), vec![]);

    X86Function {
        name: identifier.to_string(),
        lead_block: this_lead.clone(),
        tail_block: this_tail.clone(),
        blocks,
        stack_size: 0,
    }
}

/// This function is responsible for ensuring that all paths lead back to
/// the tail block.
fn si_stmt(
    exp: &BasedAstNode,
    current_block: &mut String,
    blocks: &mut OrderedHashMap<String, Vec<X86Instr>>,
) {
    use AstNode::*;
    match exp.as_ref() {
        Declaration { identifier, rhs } => {
            let (mut prefix, arg) = si_expr(rhs);
            prefix.push(X86Instr::Movq(arg, X86Arg::Var(identifier.to_string())));
            blocks.get_mut(current_block).unwrap().extend(prefix);
        }
        Assignment { lhs, rhs } => {
            let (mut prepare_value, arg) = si_expr(rhs);
            let Variable { identifier } = lhs.as_ref() else {
                dbg!(lhs);
                panic!("Assignments only should assign to variables.");
            };

            prepare_value.extend([X86Instr::Movq(arg, X86Arg::Var(identifier.clone()))]);
            blocks.get_mut(current_block).unwrap().extend(prepare_value);
        }
        FunctionCall { function, ref args } => {
            let Variable { identifier } = function.as_ref() else {
                panic!("Non identifier functions not implemented. (Or allowed?)");
            };
            assert_eq!(identifier, "print", "Only print function is implemented.");

            let (prefix, args) = si_expr(args);

            blocks.get_mut(current_block).unwrap().extend([
                X86Instr::Movq(args, X86Arg::Reg("rdi")),
                X86Instr::Callq("print_int".to_string()),
            ])
        }
        Block { stmts } => {
            let this_lead = next_label();
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
            let then_label = next_label();
            let else_label = next_label();
            let cont_label = next_label();
            assert_ne!(&then_label, &else_label);
            blocks
                .get_mut(current_block)
                .unwrap()
                .extend(condition_instrs);
            blocks.get_mut(current_block).unwrap().extend([
                X86Instr::Cmpq(condition, X86Arg::Imm(0)),
                X86Instr::Je(else_label.clone()),
                X86Instr::Jmp(then_label.clone()),
            ]);

            *current_block = then_label.clone();
            blocks.insert(then_label.clone(), Vec::new());
            si_stmt(then_blk, current_block, blocks);
            blocks
                .get_mut(current_block)
                .unwrap()
                .push(X86Instr::Jmp(cont_label.clone()));

            if let Some(else_blk) = else_blk {
                blocks.insert(else_label.clone(), Vec::new());
                *current_block = else_label.clone();
                si_stmt(else_blk, current_block, blocks);
                blocks
                    .get_mut(current_block)
                    .unwrap()
                    .push(X86Instr::Jmp(cont_label.clone()));
            }

            blocks.insert(cont_label.clone(), Vec::new());
            *current_block = cont_label.clone();
        }
        WhileStmt {
            begin_blk,
            condition,
            body_blk,
        } => {
            let (condition_instrs, condition) = si_expr(condition);
            let begin_label = next_label();
            let check_label = next_label();
            let body_label = next_label();
            let cont_label = next_label();
            blocks
                .get_mut(current_block)
                .unwrap()
                .extend([X86Instr::Jmp(begin_label.clone())]);

            blocks.insert(begin_label.clone(), vec![]);
            *current_block = begin_label.clone();
            si_stmt(begin_blk, current_block, blocks);
            blocks
                .get_mut(current_block)
                .unwrap()
                .extend([
                    X86Instr::Comment("proceed to while check".to_string()),
                    X86Instr::Jmp(check_label.clone()),
                ]);

            blocks.insert(
                check_label.clone(),
                vec![
                    X86Instr::Cmpq(condition, X86Arg::Imm(0)),
                    X86Instr::Comment("Break while loop".to_string()),
                    X86Instr::Je(cont_label.clone()),
                    X86Instr::Comment("To while body".to_string()),
                    X86Instr::Jmp(body_label.clone()),
                ],
            );

            blocks.insert(body_label.clone(), vec![]);
            *current_block = body_label.clone();
            si_stmt(body_blk, current_block, blocks);
            blocks
                .get_mut(current_block)
                .unwrap()
                .extend([
                    X86Instr::Comment("To while begin".to_string()),
                    X86Instr::Jmp(begin_label.clone()),
                ]);

            blocks.insert(cont_label.clone(), Vec::new());
            *current_block = cont_label.clone();
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
        LiteralNumber(val) => (vec![], X86Arg::Imm(*val as u64)),
        LiteralBool(val) => (vec![], X86Arg::Imm(*val as u64)),
        Variable { identifier } => (vec![], X86Arg::Var(identifier.clone())),
        Not(val) => {
            let tmp = X86Arg::Var(new_var_name());
            let mut instrs = vec![];
            let (prefix, val) = si_expr(val);
            instrs.extend(prefix);
            instrs.extend([
                X86Instr::Movq(val.clone(), tmp.clone()),
                X86Instr::Xorq(X86Arg::Imm(1), tmp.clone()),
            ]);
            (instrs, tmp.clone())
        }
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
            instrs.push(X86Instr::Movq(res, tmp.clone()));
            instrs.extend(prefix2);
            instrs.push(X86Instr::Addq(res2, tmp.clone()));
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
            instrs.push(X86Instr::Movq(res, tmp.clone()));
            instrs.extend(prefix2);
            instrs.push(X86Instr::Subq(res2, tmp.clone()));
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
            instrs.push(X86Instr::Movq(res, tmp.clone()));
            instrs.extend(prefix2);
            instrs.push(X86Instr::Imulq(res2, tmp.clone()));
            (instrs, tmp)
        }
        BinOp { op, lhs, rhs } => {
            let (prefix, res1) = si_expr(lhs);
            let (prefix2, res2) = si_expr(rhs);
            let mut instrs = vec![];
            instrs.extend(prefix);
            instrs.extend(prefix2);
            instrs.push(X86Instr::Cmpq(res2.clone(), res1.clone()));
            use BinOperation::*;
            use X86Instr::*;
            let tmp = X86Arg::Var(new_var_name());
            let al = X86Arg::Reg("al");
            let rax = X86Arg::Reg("rax");

            instrs.extend(match op {
                Eq => vec![Sete(al.clone()), Movzbq(al.clone(), tmp.clone())],
                Lt => vec![Setl(al.clone()), Movzbq(al.clone(), tmp.clone())],
                LEq => vec![Setle(al.clone()), Movzbq(al.clone(), tmp.clone())],
                Gt => vec![Setg(al.clone()), Movzbq(al.clone(), tmp.clone())],
                GEq => vec![Setge(al.clone()), Movzbq(al.clone(), tmp.clone())],
                NEq => vec![Setne(al.clone()), Movzbq(al.clone(), tmp.clone())],
                And => vec![
                    Movq(res1.clone(), tmp.clone()),
                    Andq(res2.clone(), tmp.clone()),
                ],
                Or => vec![
                    Movq(res1.clone(), tmp.clone()),
                    Orq(res2.clone(), tmp.clone()),
                ],
                _ => todo!("{:?}", op),
            });
            (instrs, tmp)
        }
        _ => todo!("Unimplemented si_expr {:?}", *exp.as_ref()),
    }
}
