#![allow(clippy::useless_format)]

use std::collections::{HashMap, HashSet};

use crate::parsing::{
    parsing::{AstNode, BasedAstNode, BinOperation},
    FileAnal,
};
use AstNode::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Type {
    Unit,
    Int,
    Bool,
    Tuple(Vec<Type>),
    Callable(Vec<Type>, Box<Type>),
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

/// Returns a list of variables storing tuples
pub fn type_check(file: &FileAnal) -> Result<HashSet<String>, Vec<String>> {
    let mut tuple_vars = HashSet::<String>::new();
    let mut file_scope = ScopeChain::<Type>::new();
    let mut errors = Vec::<String>::new();

    // Seed scope
    file_scope.add("print", Type::Callable(vec![Type::Int], Box::new(Type::Unit)));

    // Top level pass for signatures
    let mut defined_functions = HashMap::<String, Type>::new();
    for construct in &file.asts {
        if let FunctionDecl { identifier, .. } = construct.as_ref() {
            let Ok(sig) = function_signature(construct) else {
                return Err(vec![format!(
                    "Function signature {} is malformed.",
                    identifier
                )]);
            };
            defined_functions.insert(sig.0.clone(), sig.1.clone());
            file_scope.add(&sig.0, sig.1);
        }
    }

    // Check function bodies
    for construct in &file.asts {
        if let FunctionDecl { identifier, .. } = construct.as_ref() {
            dbg!(&construct);
            let mut func_scope = file_scope.subscope();
            let res = check_func_body(construct, &mut func_scope, &mut tuple_vars);
            if let Err(msg) = res {
                errors.push(msg)
            }
        }
    }

    if !errors.is_empty() {
        return Err(errors)
    }

    Ok(tuple_vars)
}

fn function_signature(func: &BasedAstNode) -> TypeCheckResult<(String, Type)> {
    let FunctionDecl {
        identifier,
        args,
        ret_ty,
        ..
    } = func.as_ref()
    else {
        todo!();
    };

    fn resolve_static_type(name: &str) -> TypeCheckResult<Type> {
        match name {
            "int" => Ok(Type::Int),
            "bool" => Ok(Type::Bool),
            _ => return Err(format!("Unrecognized type {}", name)),
        }
    }

    let mut args_t = vec![];
    for param in args {
        match param.ty {
            None => {
                return Err(format!(
                    "Parameter {} has implicity type which is not supported.",
                    param.name
                ))
            }
            Some(ty) => args_t.push(resolve_static_type(ty)?),
        }
    }
    let ret_ty: TypeCheckResult<Type> = ret_ty.map(resolve_static_type).unwrap_or(Ok(Type::Unit));
    Ok((
        identifier.to_string(),
        Type::Callable(args_t, Box::new(ret_ty?)),
    ))
}

fn check_func_body(
    func: &BasedAstNode,
    scope: &mut ScopeChain<Type>,
    tuple_vars: &mut HashSet<String>,
) -> TypeCheckResult<()> {
    let FunctionDecl {
        identifier, body, ..
    } = func.as_ref()
    else {
        panic!()
    };

    let mut body_scope = scope.subscope();

    let Block { .. } = body.as_ref() else {
        todo!("Non-block function bodies not implemented.")
    };

    // Blocks have a return type derived from any return statements the contain.
    let (_, block_ret_t) = check_expr(body, &mut body_scope, tuple_vars)?;

    let Type::Callable(args, ret_ty) = scope.resolve(identifier).unwrap() else {
        panic!();
    };
    if **ret_ty != block_ret_t {
        return Err(format!(
            "Function body does not return the type declared in the signature: expected {:?}, found {:?}",
            ret_ty, block_ret_t
        ));
    }

    Ok(())
}

/// Evaluates the expression's direct type and returned type.
///
///     return 5 -> (Unit, Int)
///
/// {
///   return 5
/// } -> (Unit, 6)
///
///
/// 5 -> (Int, Unit)
///
fn check_expr(
    stmt: &BasedAstNode,
    scope: &mut ScopeChain<Type>,
    tuple_vars: &mut HashSet<String>,
) -> TypeCheckResult<(Type, Type)> {
    match stmt.as_ref() {
        WhileStmt { condition, .. } => {
            let (condition_t, ret_t) = check_expr(condition, scope, tuple_vars)?;
            if condition_t != Type::Bool {
                return Err(format!(
                    "Condition in while statement must have type bool, not {:?}",
                    condition_t
                ));
            }
            Ok((Type::Unit, ret_t))
        }
        IfStmt { condition, .. } => {
            let (condition_t, ret_t) = check_expr(condition, scope, tuple_vars)?;
            if condition_t != Type::Bool {
                return Err(format!(
                    "Condition in if statement must have type bool, not {:?}",
                    condition_t
                ));
            }

            Ok((Type::Unit, ret_t))
        }
        EmptyParens => Ok((Type::Unit, Type::Unit)),
        Return { value } => {
            if value.is_none() {
                Ok((Type::Unit, Type::Unit))
            } else {
                let ret_ty = check_expr(value.as_ref().unwrap(), scope, tuple_vars)?;
                Ok((Type::Unit, ret_ty.0))
            }
        },
        LiteralTuple { elements } => {
            let mut el_types = vec![];
            for el in elements {
                let (t, ret_t) = check_expr(el, scope, tuple_vars)?;
                el_types.push(t);
            }

            Ok((Type::Tuple(el_types), Type::Unit))
        }
        Variable { identifier } => {
            return Ok((check_variable(stmt, scope)?, Type::Unit));
        }
        Assignment { lhs, rhs } => {
            let lhs_t = check_variable(lhs, scope)?;
            let (rhs_t, ret_t) = check_expr(rhs, scope, tuple_vars)?;
            let AstNode::Variable { identifier } = lhs.as_ref() else {
                todo!("Can we assign to things that aren't variables?")
            };

            if lhs_t != rhs_t {
                return Err(format!(
                    "Cannot assign {:?} to variable of type {:?}",
                    rhs_t, rhs_t
                ));
            }

            if matches!(rhs_t, Type::Tuple(..)) {
                tuple_vars.insert(identifier.clone());
            }
            scope.add(identifier, rhs_t.clone());

            return Ok((Type::Unit, ret_t));
        }
        Block { stmts } => {
            let mut block_ret_t = Type::Unit;
            for stmt in stmts {
                let (exp_t, ret_t) = check_expr(stmt, scope, tuple_vars)?;
                if ret_t != block_ret_t && block_ret_t != Type::Unit {
                    return Err(format!("Incompatible return types: {:?}, {:?}", ret_t, block_ret_t))
                }
                block_ret_t = ret_t;
            }
            return Ok((Type::Unit, block_ret_t));
        }
        FunctionCall {
            function,
            args_tuple: args,
        } => {
            let Type::Callable(args, res) = check_expr(function, scope, tuple_vars)?.0 else {
                return Err("Can only call values of type Callable.".to_string());
            };
            return Ok((*res.clone(), Type::Unit));
        }
        FunctionDecl {
            identifier, body, ..
        } => {
            let duplicate = scope.add(identifier, function_signature(stmt)?.1);
            if duplicate {
                return Err(format!("Identifier {} declared twice.", identifier));
            }
            return Ok((Type::Unit, Type::Unit));
        }
        LiteralNumber(_) => return Ok((Type::Int, Type::Unit)),
        LiteralBool(_) => return Ok((Type::Bool, Type::Unit)),
        Not(expr) => {
            return Ok((check_unaryop(&BinOperation::Bang, expr, scope, tuple_vars)?, Type::Unit));
        }
        BinOp { op, lhs, rhs } => {
            return Ok((check_binop(op, lhs, rhs, scope, tuple_vars)?, Type::Unit));
        }
        Declaration { identifier, rhs } => {
            let (rhs_t, _) = check_expr(rhs, scope, tuple_vars)?;
            scope.add(identifier, rhs_t.clone());
            if matches!(rhs_t, Type::Tuple(..)) {
                tuple_vars.insert(identifier.to_string());
            }

            return Ok((Type::Unit, Type::Unit));
        }
    }
}

fn check_unaryop(
    op: &BinOperation,
    expr: &BasedAstNode,
    scope: &mut ScopeChain<Type>,
    tuple_vars: &mut HashSet<String>,
) -> TypeCheckResult<Type> {
    use BinOperation::*;
    match op {
        Bang => {
            let (expr_t, _) = check_expr(expr, scope, tuple_vars)?;
            if expr_t != Type::Bool {
                return Err("! can only be applied to booleans.".to_string());
            }
            return Ok(Type::Bool);
        }
        x => panic!("Why is {:?} in a not node?", expr),
    }
}

fn check_binop(
    op: &BinOperation,
    lhs: &BasedAstNode,
    rhs: &BasedAstNode,
    scope: &mut ScopeChain<Type>,
    tuple_vars: &mut HashSet<String>,
) -> TypeCheckResult<Type> {
    use BinOperation::*;
    match op {
        Bang => panic!("Why is bang in a binop node?"),
        LiteralJoinTuple => panic!("This shouldn't have escaped the parsing code."),
        And | Or => {
            let (lhs_t, _) = check_expr(lhs, scope, tuple_vars)?;
            let (rhs_t, _) = check_expr(rhs, scope, tuple_vars)?;

            if lhs_t != Type::Bool || rhs_t != Type::Bool {
                return Err("'and' and 'or' keywords only work with bool arguments.".to_string());
            }

            return Ok(Type::Bool);
        }
        Eq | Gt | GEq | NEq | Lt | LEq => {
            let (lhs_t, _) = check_expr(lhs, scope, tuple_vars)?;
            let (rhs_t, _) = check_expr(rhs, scope, tuple_vars)?;

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
            let (lhs_t, _) = check_expr(lhs, scope, tuple_vars)?;
            let (rhs_t, _) = check_expr(rhs, scope, tuple_vars)?;
            if lhs_t != Type::Int || rhs_t != Type::Int {
                return Err(format!(
                    "{:?} is only supported between int types, not {:?} {:?}",
                    op, lhs_t, rhs_t
                ));
            }
            return Ok(Type::Int);
        }
        Subscript => {
            let (lhs_t, _) = check_expr(lhs, scope, tuple_vars)?;
            let (rhs_t, _) = check_expr(rhs, scope, tuple_vars)?;

            if rhs_t != Type::Int {
                return Err(format!("Tuple subscripts must be integers."))
            }

            let LiteralNumber(offset) = rhs.as_ref() else {
                return Err(format!("Tuple subscripts must be integer literals."))
            };

            let Type::Tuple(args) = lhs_t else {
                return Err(format!("Only tuple types can be subscripted."))
            };

            if *offset < 0 || args.len() <= *offset as usize {
                return Err(format!("Tuple index {} is out of range {}.", offset, args.len()))
            }
            return Ok(args.get(*offset as usize).unwrap().clone())
        }
    }
}

fn check_variable(node: &BasedAstNode, scope: &mut ScopeChain<Type>) -> TypeCheckResult<Type> {
    match node.as_ref() {
        Variable { identifier } => {
            let t = scope.resolve(identifier);
            match t {
                Some(t) => Ok(t.clone()),
                None => Err(format!("Unrecognized symbol {}", &identifier)),
            }
        }
        _ => Err(format!("Expected variable but got {:?}", node)),
    }
}
