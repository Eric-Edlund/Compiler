use crate::parsing::parsing::{AstNode::*, BinOperation};
use crate::parsing::{parsing::BasedAstNode, FileAnal};
use lazy_static::lazy_static;
use std::{ops::DerefMut, sync::Mutex};

lazy_static! {
    static ref sym_id: Mutex<u32> = Mutex::new(0);
}

fn temp_var_sym() -> String {
    let mut id = sym_id.lock().unwrap();
    *id += 1;
    format!("tmp_{}", *id)
}

// Takes expressions and flattens them into atomic operations on temporary variables
// which can be represented in assembly.
pub fn remove_complex_operands(units: &mut FileAnal) {
    for unit in &mut units.asts {
        rco(unit);
    }
}

// Takes a node, may add new nodes.
// Equivalent to rco_stmts in the python compiler
//
// Resulting expressions are flat.
// Takes a Block
fn rco(unit: &mut BasedAstNode) {
    match unit.deref_mut() {
        FunctionDecl {
            identifier, body, ..
        } => {
            let mut new_stmts = Vec::<BasedAstNode>::new();
            rco_stmt(body, &mut new_stmts);
        }
        _ => panic!("Unimplemented {:?}", *unit.as_ref()),
    };

    assert_rco_invariant(
        Context {
            parent_is_binop: false,
        },
        &*unit,
    );
}

#[derive(Clone, Copy, Debug)]
struct Context {
    parent_is_binop: bool,
}

fn assert_rco_invariant(ctx: Context, unit: &BasedAstNode) {
    let mut ctx = ctx;
    match unit.as_ref() {
        BinOp { op, lhs, rhs } => {
            assert!(!ctx.parent_is_binop, "Binops may not nest after rco.");
            ctx.parent_is_binop = true;
            assert_rco_invariant(ctx, lhs);
            assert_rco_invariant(ctx, rhs);
        }
        x => {
            ctx.parent_is_binop = false;
            let children = unit.as_ref().child_nodes();
            for child in children {
                assert_rco_invariant(ctx, &child);
            }
        }
    }
}

// Accepts a unit to flatten and a list of statements to append intermediate
// actions to. The unit is mutated in place.
fn rco_stmt<'a>(unit: &mut BasedAstNode<'a>, new_stmts: &mut Vec<BasedAstNode<'a>>) {
    match unit.deref_mut() {
        Block { ref mut stmts } => {
            let mut new_block_stmts = Vec::new();
            for stmt in &mut *stmts {
                rco_stmt(stmt, &mut new_block_stmts);
                new_block_stmts.push(stmt.clone());
            }
            *stmts = new_block_stmts;
        }
        Assignment { lhs, rhs } => {
            let result = rco_expr(rhs, new_stmts);
            *rhs = result.clone();
        }
        Declaration { identifier, rhs } => {
            let new_rhs = rco_expr(rhs, new_stmts);
            *rhs = new_rhs;
        }
        FunctionCall {
            function,
            args_tuple: args,
        } => {
            let Variable { .. } = function.as_ref() else {
                panic!("Function calls must call variables.");
            };
            *unit = rco_expr(unit, new_stmts);
        }
        IfStmt {
            ref mut condition,
            ref mut then_blk,
            ref mut else_blk,
        } => {
            let atomic_condition = rco_expr(condition, new_stmts);
            *condition = atomic_condition;
            rco_stmt(then_blk, new_stmts);
            if let Some(else_blk) = else_blk {
                rco_stmt(else_blk, new_stmts);
            }
        }
        WhileStmt {
            ref mut begin_blk,
            ref mut condition,
            ref mut body_blk,
        } => {
            let Block { stmts: begin_stmts } = begin_blk.deref_mut() else {
                panic!("AstNode::WhileStmt.begin_blk must always be a AstNode::Block")
            };
            assert_eq!(
                begin_stmts.len(),
                0,
                "While begin block cannot be directly defined in the language's syntax."
            );
            let atomic_condition = rco_expr(condition, begin_stmts);
            *condition = atomic_condition;
            rco_stmt(body_blk, new_stmts);
        }
        Return { ref mut value } => {
            if let Some(v) = value {
                let ret_val = rco_expr(v, new_stmts);
                *value = Some(ret_val.clone())
            }
        }
        _ => panic!("Unimplemented {:?}", *unit.as_ref()),
    }
}

/// Take the possibly nested expression and flatten it into intermediate assignments
/// and temporaries. Add the calculating statements to new_stmts and return a new
/// variable storing the calculated result.
fn rco_expr<'a>(
    unit: &BasedAstNode<'a>,
    new_stmts: &mut Vec<BasedAstNode<'a>>,
) -> BasedAstNode<'a> {
    match unit.as_ref() {
        Variable { .. } | LiteralNumber(_) | LiteralBool(_) => unit.clone(),
        LiteralTuple { elements } => {
            let new_elements = elements
                .iter()
                .map(|arg| rco_expr(arg, new_stmts))
                .collect();

            LiteralTuple {
                elements: new_elements,
            }
            .into()
        }
        FunctionCall {
            function,
            args_tuple,
        } => {
            assert!(matches!(args_tuple.as_ref(), LiteralTuple { .. }));
            let new_args = rco_expr(args_tuple, new_stmts);

            FunctionCall {
                function: function.clone(),
                args_tuple: new_args,
            }
            .into()
        }
        Not(expr) => {
            let expr = rco_expr(expr, new_stmts);
            let tmp: BasedAstNode = Variable {
                identifier: temp_var_sym(),
            }
            .into();
            new_stmts.push(
                Assignment {
                    lhs: tmp.clone(),
                    rhs: Not(expr).into(),
                }
                .into(),
            );
            tmp
        }
        BinOp { lhs, rhs, op } => {
            use BinOperation::*;
            match op {
                Bang => panic!("Why is bang in a binary operation?"),
                LiteralJoinTuple { .. } => panic!("This should never leave the parsing section"),
                Eq | LEq | GEq | Gt | Lt | NEq | Add | Sub | Mult | Div | Call | And | Or => {
                    let lhs = rco_expr(lhs, new_stmts);
                    let rhs = rco_expr(rhs, new_stmts);
                    let tmp: BasedAstNode = Variable {
                        identifier: temp_var_sym(),
                    }
                    .into();
                    new_stmts.push(
                        Assignment {
                            lhs: tmp.clone(),
                            rhs: BasedAstNode::from(BinOp { lhs, rhs, op: *op }.clone()),
                        }
                        .into(),
                    );
                    tmp
                }
                Assign => {
                    let rhs = rco_expr(rhs, new_stmts);
                    new_stmts.push(
                        Assignment {
                            lhs: lhs.clone(),
                            rhs,
                        }
                        .into(),
                    );
                    BasedAstNode::from(EmptyParens)
                }
                Subscript => {
                    let lhs = rco_expr(lhs, new_stmts);
                    let rhs = rco_expr(rhs, new_stmts);
                    let tmp: BasedAstNode = Variable {
                        identifier: temp_var_sym(),
                    }.into();
                    new_stmts.push(
                        Assignment {
                            lhs: tmp.clone(),
                            rhs: BinOp {
                                lhs,
                                rhs,
                                op: *op,
                            }.into(),
                        }
                        .into(),
                    );
                    tmp
                }
                _ => todo!("{:?}", op),
            }
        }
        x => {
            todo!("{:?} in {:?}", x, unit)
        }
    }
}
