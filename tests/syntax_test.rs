use std::cell::RefCell;

use compiler::parsing::{
    build_ast,
    parsing::{AstNode, BinOperation, ParseError},
};

static PRGM_1: &str = "
// Comment
fn main() {
    let x = 1;
    x = x + 1;
    return x;
}
";

static PRGM_2: &str = "
fn main() {
    if x + 2 == 4 {
        print();
    } else {
        x = 3;
    }
}
";

#[test]
fn parse_1() {
    let errs = RefCell::new(Vec::<ParseError>::new());

    let anal = build_ast(PRGM_1, |e| errs.borrow_mut().push(e));
    dbg!(&errs);

    let fn1: &AstNode = anal.asts[0].as_ref();
    let AstNode::FunctionDecl {
        identifier, body, ..
    } = fn1
    else {
        panic!();
    };

    assert_eq!(identifier, &"main");

    let AstNode::Block { stmts, .. } = body.as_ref() else {
        panic!();
    };

    let AstNode::Declaration { identifier, rhs } = stmts[0].as_ref() else {
        panic!();
    };

    assert_eq!(identifier, &"x");
    let AstNode::LiteralNumber(n) = rhs.as_ref() else {
        panic!();
    };
    assert_eq!(*n, 1);

    dbg!(&anal.src_tokens);
    dbg!(&anal.asts);

    dbg!(errs);

    assert_eq!(anal.asts.len(), 1);
}

/// With if
#[test]
fn parse_2() {
    use AstNode::*;
    let errs = RefCell::new(Vec::<ParseError>::new());

    let anal = build_ast(PRGM_2, |e| errs.borrow_mut().push(e));
    dbg!(&errs, &anal.asts);

    let fn1: &AstNode = anal.asts[0].as_ref();
    let AstNode::FunctionDecl {
        identifier, body, ..
    } = fn1
    else {
        panic!();
    };
    assert_eq!(identifier, &"main");

    let Block { stmts } = body.as_ref() else {
        panic!();
    };

    let AstNode::IfStmt {
        condition,
        then_blk,
        else_blk,
    } = stmts[0].as_ref()
    else {
        panic!("{:?}", stmts[0].as_ref());
    };

    let BinOp { op, lhs, rhs } = condition.as_ref() else {
        panic!();
    };
    assert_eq!(*op, BinOperation::Eq);

    let BinOp {op: opleft, lhs, rhs} = lhs.as_ref() else {
        panic!();
    };
    assert_eq!(*opleft, BinOperation::Add);

    let Block { stmts } = then_blk.as_ref() else {
        panic!();
    };

    assert_eq!(stmts.len(), 1);

    let FunctionCall { function, args } = stmts[0].as_ref() else {
        panic!();
    };

    let Block {stmts} = else_blk.as_ref().unwrap().as_ref() else {
        panic!();
    };
}
