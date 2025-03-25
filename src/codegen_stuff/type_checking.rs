use std::collections::HashMap;

use crate::parsing::{
    parsing::{AstNode, BasedAstNode, BinOperation},
    FileAnal,
};
use AstNode::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
enum Type<'a> {
    Unit,
    Int,
    Bool,
    Callable(&'a Type<'a>, &'a Type<'a>),
    Unimplemented,
}

type TypeCheckResult<T> = Result<T, String>;

struct ScopeChain<'a, T> {
    scope: HashMap<String, T>,
    parent: Option<&'a ScopeChain<'a, T>>,
}

impl<'a, T> ScopeChain<'a, T> {
    fn new() -> Self {
        ScopeChain {
            scope: HashMap::new(),
            parent: None,
        }
    }

    fn subscope<'b: 'a>(&'b self) -> Self {
        ScopeChain {
            scope: HashMap::new(),
            parent: Some(self),
        }
    }

    fn resolve(&self, symbol: &str) -> Option<&T> {
        if let Some(res) = self.scope.get(symbol) {
            return Some(res);
        }

        if let Some(parent) = self.parent {
            parent.resolve(symbol)
        } else {
            None
        }
    }

    // Returns true if the symbol already existed.
    fn add(&mut self, symbol: &str, val: T) -> bool {
        self.scope.insert(symbol.to_string(), val).is_some()
    }
}

pub fn type_check(file: &FileAnal) -> Result<(), Vec<String>> {
    let file_scope = ScopeChain::<Type>::new();
    let mut errors = Vec::<String>::new();
    for construct in &file.asts {
        let mut construct_scope = file_scope.subscope();
        let res = top_lvl_check_stmt(construct, &mut construct_scope);
        if let Err(msg) = res {
            errors.push(msg)
        }
    }

    Ok(())
}

fn top_lvl_check_stmt<'a>(
    stmt: &BasedAstNode,
    scope: &mut ScopeChain<Type<'a>>,
) -> TypeCheckResult<Type<'a>> {
    match stmt.as_ref() {
        FunctionDecl { identifier, body } => {
            let duplicate = scope.add(identifier, Type::Unit);
            if duplicate {
                return Err(format!("Duplicate identifier: {}", identifier));
            }

            return check_expr(body, scope);
        }
        _ => todo!(),
    }
}

fn check_expr<'a>(
    stmt: &BasedAstNode,
    scope: &mut ScopeChain<Type<'a>>,
) -> TypeCheckResult<Type<'a>> {
    match stmt.as_ref() {
        WhileStmt { condition, .. } => {
            let condition_t = check_expr(condition, scope)?;
            if condition_t != Type::Bool {
                return Err(format!(
                    "Condition in while statement must have type bool, not {:?}",
                    condition_t
                ));
            }
            Ok(Type::Unit)
        }
        IfStmt { condition, .. } => {
            let condition_t = check_expr(condition, scope)?;
            if condition_t != Type::Bool {
                return Err(format!(
                    "Condition in if statement must have type bool, not {:?}",
                    condition_t
                ));
            }

            Ok(Type::Unit)
        }
        EmptyParens => Ok(Type::Unit),
        Return { .. } => Ok(Type::Unit),
        CommaList { .. } => todo!(),
        Variable { identifier } => {
            return check_variable(stmt, scope);
        }
        Assignment { lhs, rhs } => {
            let lhs_t = check_variable(lhs, scope)?;
            let rhs_t = check_expr(rhs, scope)?;

            if lhs_t != rhs_t {
                return Err(format!(
                    "Cannot assign {:?} to variable of type {:?}",
                    rhs_t, rhs_t
                ));
            }

            return Ok(Type::Unit);
        }
        Block { stmts } => {
            for stmt in stmts {
                check_expr(stmt, scope)?;
            }
            return Ok(Type::Unit);
        }
        FunctionCall { function, args } => {
            let Type::Callable(args, res) = check_expr(function, scope)? else {
                return Err(format!("Can only call values of type Callable."));
            };
            return Ok(*res);
        }
        FunctionDecl { identifier, body } => {
            let duplicate = scope.add(
                identifier,
                Type::Callable(&Type::Unimplemented, &Type::Unit),
            );
            if duplicate {
                return Err(format!("Identifier {} declared twice.", identifier));
            }
            return Ok(Type::Unit);
        }
        LiteralNumber(_) => return Ok(Type::Int),
        LiteralBool(_) => return Ok(Type::Bool),
        Not(expr) => {
            return check_unaryop(&BinOperation::Bang, expr, scope);
        }
        BinOp { op, lhs, rhs } => {
            return check_binop(op, lhs, rhs, scope);
        }
        Declaration { identifier, rhs } => {
            let rhs_t = check_expr(rhs, scope)?;

            return Ok(Type::Unit);
        }
    }
}

fn check_unaryop<'a>(
    op: &BinOperation,
    expr: &BasedAstNode,
    scope: &mut ScopeChain<Type<'a>>
) -> TypeCheckResult<Type<'a>> {
    use BinOperation::*;
    match op {
        Bang => {
            let expr_t = check_expr(expr, scope)?;
            if expr_t != Type::Bool {
                return Err(format!("! can only be applied to booleans."))
            }
            return Ok(Type::Bool)
        }
        x => panic!("Why is {:?} in a not node?", expr)
    }

}

fn check_binop<'a>(
    op: &BinOperation,
    lhs: &BasedAstNode,
    rhs: &BasedAstNode,
    scope: &mut ScopeChain<Type<'a>>,
) -> TypeCheckResult<Type<'a>> {
    use BinOperation::*;
    match op {
        Bang => panic!("Why is bang in a binop node?"),
        Eq | Gt | GEq | NEq | Lt | LEq => {
            let lhs_t = check_expr(lhs, scope)?;
            let rhs_t = check_expr(rhs, scope)?;

            match lhs_t {
                Type::Bool | Type::Int => {}
                _ => todo!(),
            }

            if lhs_t == rhs_t {
                return Ok(Type::Bool);
            } else {
                return Err(format!(
                    "Cannot compare different types: {:?} {:?}",
                    lhs_t, rhs_t
                ));
            }
        }
        Assign => return Ok(Type::Unit),
        Call => {
            // TODO
            return Ok(Type::Unit);
        }
        Mult | Div | Add | Sub => {
            let lhs_t = check_expr(lhs, scope)?;
            let rhs_t = check_expr(rhs, scope)?;
            if lhs_t != Type::Int || rhs_t != Type::Int {
                return Err(format!(
                    "{:?} is only supported between int types, not {:?} {:?}",
                    op, lhs_t, rhs_t
                ));
            }
            return Ok(Type::Int);
        }
    }
}

fn check_variable<'a>(
    node: &BasedAstNode,
    scope: &mut ScopeChain<Type<'a>>,
) -> TypeCheckResult<Type<'a>> {
    match node.as_ref() {
        Variable { identifier } => {
            let t = scope.resolve(identifier);
            match t {
                Some(t) => Ok(*t),
                None => Err(format!("Unrecognized symbol {}", &identifier)),
            }
        }
        _ => Err(format!("Expected variable but got {:?}", node)),
    }
}
