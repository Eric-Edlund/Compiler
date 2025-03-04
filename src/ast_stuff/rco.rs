use std::{ops::DerefMut, sync::Mutex};

use lazy_static::lazy_static;
use crate::parsing::{parsing::BasedAstNode, FileAnal};
use crate::parsing::parsing::AstNode::*;

lazy_static! {
    static ref sym_id: Mutex<u32> = Mutex::new(0);
}

fn temp_var_sym() -> String {
    let mut id = sym_id.lock().unwrap();
    *id += 1;
    format!("tmp_{}", *id)
}

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
            identifier,
            body,
        } => {
            assert_eq!(*identifier, "main");
            rco(body);
        }
        Block {ref mut stmts} => {
            let mut new_stmts = Vec::<BasedAstNode>::new();
            for stmt in &*stmts {
                let replacement = rco_stmt(stmt, &mut new_stmts);
                new_stmts.push(replacement);
            }
            *stmts = new_stmts;
        }
        _ => panic!("Unimplemented {:?}", *unit.as_ref())
    }
}

fn rco_stmt<'a>(unit: &BasedAstNode<'a>, new_stmts: &mut Vec<BasedAstNode<'a>>) -> BasedAstNode<'a> {
    match unit.as_ref() {
        Assignment { lhs, rhs } => {
            return BasedAstNode::from(Assignment { lhs: lhs.clone(), rhs: rco_expr(rhs, new_stmts) })
        }
        Declaration { identifier, rhs } => {
            let new_rhs = rco_expr(rhs, new_stmts);
            return BasedAstNode::from(Declaration {identifier, rhs: new_rhs})
        }
        FunctionCall { function, args } => {
            let Variable { identifier } = function.as_ref() else {
                panic!("Function calls must call variables.");
            };
            let args = rco_expr(args, new_stmts);
            return BasedAstNode::from(FunctionCall {function: function.clone(), args})
        }
        _ => panic!("Unimplemented {:?}", *unit.as_ref())
    }
}


fn rco_expr<'a>(unit: &BasedAstNode<'a>, new_stmts: &mut Vec<BasedAstNode<'a>>) -> BasedAstNode<'a>{
    match unit.as_ref() {
        Variable {..} | LiteralNumber(_) => unit.clone(),
        BinOp {lhs, rhs, ..} => {
            let lhs = rco_expr(lhs, new_stmts);
            let rhs = rco_expr(rhs, new_stmts);
            let tmp: BasedAstNode = Variable {identifier: temp_var_sym()}.into();
            new_stmts.push(Assignment {lhs: tmp.clone(), rhs: unit.clone()}.into());
            tmp
        }
        _ => {panic!()}
    }
}
