use super::lexing::Token;
use super::lexing::TokenType;
use std::cell::RefCell;
use std::ops::Deref;
use std::ops::DerefMut;

// Values act as operator presidence
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOperation {
    Assign,
    Call,
    Mult,
    Div,
    Add,
    Sub,
    Eq,
    Lt,
    Gt,
    LEq,
    GEq,
    NEq,
}

#[derive(Debug, Clone)]
pub enum AstNode<'a> {
    FunctionDecl {
        identifier: &'a str,
        body: BasedAstNode<'a>,
    },
    LiteralNumber(i32),
    BinOp {
        op: BinOperation,
        lhs: BasedAstNode<'a>,
        rhs: BasedAstNode<'a>,
    },
    Block {
        stmts: Vec<BasedAstNode<'a>>,
    },
    Declaration {
        identifier: &'a str,
        rhs: BasedAstNode<'a>,
    },
    Assignment {
        lhs: BasedAstNode<'a>,
        rhs: BasedAstNode<'a>,
    },
    FunctionCall {
        function: BasedAstNode<'a>,
        args: BasedAstNode<'a>,
    },
    Variable {
        identifier: String,
    },
    CommaList {
        elements: Vec<BasedAstNode<'a>>
    },
    EmptyParens,
    Return {value: Option<BasedAstNode<'a>>},
    IfStmt {
        condition: BasedAstNode<'a>,
        then_blk: BasedAstNode<'a>,
        else_blk: Option<BasedAstNode<'a>>,
    },
}

use AstNode::*;

/// Also keeps a reference to the source token.
#[derive(Debug, Clone)]
pub struct BasedAstNode<'a> {
    inner: Box<AstNode<'a>>,
    token: Option<&'a Token>,
}

impl<'a> From<AstNode<'a>> for BasedAstNode<'a> {
    fn from(value: AstNode<'a>) -> Self {
        BasedAstNode {
            inner: Box::new(value),
            token: None,
        }
    }
}

impl<'a> AsRef<AstNode<'a>> for BasedAstNode<'a> {
    fn as_ref(&self) -> &AstNode<'a> {
        self.inner.as_ref()
    }
}

impl<'a> Deref for BasedAstNode<'a> {
    type Target = AstNode<'a>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a> DerefMut for BasedAstNode<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    unexpected_token: Option<Token>,
    context: Option<String>,
    need_another_token: bool,
    was_expecting: Option<TokenType>,
}

impl ParseError {
    fn unexpected(tok: &Token, context: impl Into<String>, expected: Option<TokenType>) -> Self {
        Self {
            unexpected_token: Some(tok.clone()),
            context: Some(context.into()),
            need_another_token: false,
            was_expecting: expected,
        }
    }
    fn missing(context: impl Into<String>) -> Self {
        Self {
            unexpected_token: None,
            context: Some(context.into()),
            need_another_token: true,
            was_expecting: None,
        }
    }
    fn other (context: impl Into<String>) -> Self {
        Self {
            unexpected_token: None,
            context: Some(context.into()),
            need_another_token: false,
            was_expecting: None,
        }
    }
}

type PResult<T> = Result<T, ParseError>;

pub fn parse_file<'a, ErrConsumer>(
    tokens: &'a [Token],
    err_cb: ErrConsumer,
) -> Vec<BasedAstNode<'a>>
where
    ErrConsumer: Fn(ParseError),
{
    let mut i = 0 as usize;

    let mut result = Vec::<BasedAstNode<'a>>::new();

    while i < tokens.len() {
        let token = &tokens[i];
        i += 1;
        match token.ty {
            TokenType::Fn => match consume_function(tokens, &mut i) {
                Err(e) => {
                    err_cb(e);
                    return result;
                }
                Ok(node) => result.push(node),
            },
            TokenType::LineComment => {}
            tok => {
                err_cb(ParseError::unexpected(token, "File top level", None));
                // TODO: Don't give up after the first problem, recover some how.
                return result;
            }
        }
    }

    result
}

fn consume_function<'a>(tokens: &'a [Token], start: &mut usize) -> PResult<BasedAstNode<'a>> {
    let iter = RefCell::new(tokens[*start - 1..].iter());
    *start -= 1;
    let start = RefCell::new(start);

    let take = || {
        **start.borrow_mut() += 1;
        let Some(tok) = iter.borrow_mut().next() else {
            return Err(ParseError::missing("Function"));
        };
        Ok(tok)
    };

    let expect = |ty: TokenType| {
        let tok = take()?;
        if tok.ty != ty {
            return Err(ParseError::unexpected(tok, "Function", Some(ty)));
        }
        Ok(tok)
    };

    let fn_tok = expect(TokenType::Fn)?;
    let ident_tok = expect(TokenType::Identifier)?;
    expect(TokenType::LParen)?;
    expect(TokenType::RParen)?;
    expect(TokenType::LBrace)?;

    **start.borrow_mut() -= 1;
    let blk = consume_block(tokens, *start.borrow_mut())?;

    return Ok(BasedAstNode::<'a> {
        inner: Box::new(FunctionDecl {
            identifier: &ident_tok.src,
            body: blk,
        }),
        token: Some(fn_tok),
    });
}

/// start should be the first token to consume, probably {
fn consume_block<'a>(tokens: &'a [Token], start: &mut usize) -> PResult<BasedAstNode<'a>> {
    dbg!("Block started with {:?}", &tokens[*start..*start + 5]);

    let start = RefCell::new(start);

    let take = || {
        let Some(tok) = tokens.get(**start.borrow()) else {
            return Err(ParseError::missing("Block"));
        };
        **start.borrow_mut() += 1;
        Ok(tok)
    };

    let expect = move |ty: TokenType| {
        let tok = take()?;
        if tok.ty != ty {
            return Err(ParseError::unexpected(tok, "Block", Some(ty)));
        }
        Ok(tok)
    };

    let lbrace_tok = expect(TokenType::LBrace)?;

    let mut stmts = Vec::<BasedAstNode>::new();

    while let Ok(tok) = take() {
        match tok.ty {
            TokenType::If => {
                let expr = consume_expression(tokens, *start.borrow_mut())?;
                let then_blk = consume_block(tokens, *start.borrow_mut())?;
                let mut else_blk = None;
                if expect(TokenType::Else).is_ok() {
                    else_blk = Some(consume_block(tokens, *start.borrow_mut())?);
                }

                stmts.push(IfStmt {
                    condition: expr,
                    then_blk,
                    else_blk,
                }.into())
            }
            TokenType::LBrace => {
                **start.borrow_mut() -= 1;
                stmts.push(consume_block(tokens, *start.borrow_mut())?);
            }
            TokenType::Let => {
                **start.borrow_mut() -= 1;
                let decl_node = consume_declaration(tokens, *start.borrow_mut())?;
                stmts.push(decl_node);
            }
            TokenType::Identifier => {
                **start.borrow_mut() -= 1;
                let expr_node = consume_expression(tokens, *start.borrow_mut())?;
                dbg!("Block consumed expression", &expr_node);
                stmts.push(expr_node);
            }
            TokenType::LineComment => {}
            TokenType::RBrace => break,
            TokenType::Return => {
                let val = match take() {
                    Err(_) => None,
                    Ok(token) => match token.ty {
                        TokenType::Semi | TokenType:: RBrace => None,
                        _ => {
                            **start.borrow_mut() -= 1;
                            Some(consume_expression(tokens, *start.borrow_mut())?)
                        },
                    }
                };
                stmts.push(BasedAstNode {
                    inner: Box::new(Return {value: val}),
                    token: Some(tok),
                })
            }
            _ => return Err(ParseError::unexpected(tok, "Inside block", None)),
        }
    }

    Ok(BasedAstNode {
        inner: Box::new(Block { stmts }),
        token: Some(lbrace_tok),
    })
}

fn consume_declaration<'a>(tokens: &'a [Token], start: &mut usize) -> PResult<BasedAstNode<'a>> {
    dbg!("Starting declaration", &tokens[*start..*start+5]);
    let start = RefCell::new(start);

    let take = || {
        let Some(tok) = tokens.get(**start.borrow()) else {
            return Err(ParseError::missing("Declaration"));
        };
        **start.borrow_mut() += 1;
        Ok(tok)
    };

    let expect = |ty: TokenType| {
        let tok = take()?;
        if tok.ty != ty {
            return Err(ParseError::unexpected(tok, "Declaration", Some(ty)));
        }
        Ok(tok)
    };

    expect(TokenType::Let)?;
    let ident_tok = expect(TokenType::Identifier)?;
    expect(TokenType::EqAssign)?;
    // **start.borrow_mut() += 3;
    let rhs = consume_expression(tokens, *start.borrow_mut())?;

    Ok(BasedAstNode::<'a> {
        inner: Box::new(Declaration {
            identifier: &ident_tok.src,
            rhs,
        }),
        token: Some(ident_tok),
    })
}

/// Expressions are only ended by semicolons or ) or {
/// This function accepts the position of the first token of the expression
/// and returns the position of the first token not consumed. This does
/// not include the semi or ), unless the expression started on a (.
fn consume_expression<'a>(tokens: &'a [Token], start: &mut usize) -> PResult<BasedAstNode<'a>> {
    if tokens.len() > *start + 5 {
        dbg!("Starting expression", &tokens[*start..*start + 5]);
    }
    let iter = tokens[*start..].iter().peekable();

    // TODO: Raise if given an operator before a value

    if tokens[*start].ty == TokenType::LParen {
        // return consume_expression
    }

    fn get_presidence(op: &BinOperation) -> u32 {
        match op {
            BinOperation::Call => 10,
            BinOperation::Mult => 8,
            BinOperation::Div => 8,
            BinOperation::Add => 5,
            BinOperation::Sub => 5,
            BinOperation::Eq => 4,
            BinOperation::Lt => 4,
            BinOperation::Gt => 4,
            BinOperation::LEq => 4,
            BinOperation::GEq => 4,
            BinOperation::NEq => 4,
            BinOperation::Assign => 2,
        }
    }

    fn apply_op<'a>(
        op: &BinOperation,
        lhs: BasedAstNode<'a>,
        rhs: BasedAstNode<'a>,
    ) -> BasedAstNode<'a> {
        match op {
            BinOperation::Call => BasedAstNode {
                inner: Box::new(FunctionCall { function: lhs, args: rhs }),
                token: None,
            },
            BinOperation::Assign => BasedAstNode {
                inner: Box::new(Assignment { lhs, rhs }),
                token: None,
            },
            BinOperation::Mult => BasedAstNode {
                inner: Box::new(BinOp {
                    op: BinOperation::Mult,
                    lhs,
                    rhs,
                }),
                token: None,
            },
            BinOperation::Div => BasedAstNode {
                inner: Box::new(BinOp {
                    op: BinOperation::Div,
                    lhs,
                    rhs,
                }),
                token: None,
            },
            BinOperation::Add => BasedAstNode {
                inner: Box::new(BinOp {
                    op: BinOperation::Add,
                    lhs,
                    rhs,
                }),
                token: None,
            },
            BinOperation::Sub => BasedAstNode {
                inner: Box::new(BinOp {
                    op: BinOperation::Sub,
                    lhs,
                    rhs,
                }),
                token: None,
            },
            BinOperation::Eq | BinOperation::NEq | BinOperation::Lt | BinOperation::Gt | BinOperation::LEq | BinOperation::GEq => BinOp {
                op: *op,
                lhs,
                rhs,
            }.into()
        }
    }

    // Presidence climbing
    let mut exprs = Vec::<BasedAstNode>::new();
    let mut ops = Vec::<BinOperation>::new();
    let mut last_token_was_op = false;

    while *start < tokens.len() {
        let tok = &tokens[*start];
        *start += 1;
        let op: Option<BinOperation> = match tok.ty {
            TokenType::Semi => {
                break
            },
            TokenType::LParen => {
                let parenthized_expr = if tokens[*start].ty == TokenType::RParen {
                    Some(BasedAstNode {
                        inner: Box::new(EmptyParens),
                        token: Some(&tokens[*start]),
                    })
                } else {
                    let r = consume_expression(tokens, start)?;
                    Some(r)
                };

                let TokenType::RParen = tokens[*start].ty else {
                    return Err(ParseError::missing("Expression, expecting )"));
                };
                *start += 1;

                if let Some(expr) = parenthized_expr {
                    exprs.push(expr);
                }

                if !last_token_was_op || ops.last().is_some_and(|x| *x == BinOperation::Call) {
                    Some(BinOperation::Call)
                } else {
                    None
                }
            },
            TokenType::LBrace => {
                *start -= 1;
                break;
            }
            TokenType::RParen => {
                *start -= 1;
                break;
            }
            TokenType::EqAssign => Some(BinOperation::Assign),
            TokenType::OpMult => Some(BinOperation::Mult),
            TokenType::OpAdd => Some(BinOperation::Add),
            TokenType::Identifier => {
                exprs.push(BasedAstNode {
                    inner: Box::new(Variable {identifier: tok.src.clone()}),
                    token: Some(tok),
                });
                None
            }
            TokenType::NumberLit => {
                exprs.push(BasedAstNode {
                    inner: Box::new(LiteralNumber(str::parse::<i32>(&tok.src).unwrap())),
                    token: Some(tok),
                });
                None
            }
            TokenType::EqCmp => Some(BinOperation::Eq),
            TokenType::NEqCmp => Some(BinOperation::NEq),
            TokenType::LCmp => Some(BinOperation::Lt),
            TokenType::GCmp => Some(BinOperation::Gt),
            TokenType::GEqCmp => Some(BinOperation::GEq),
            TokenType::LEqCmp => Some(BinOperation::LEq),
            _ => None,
        };
        last_token_was_op = op.is_some();
        dbg!("Consumed", op, &exprs, &ops);
        if let Some(ref op) = op {
            // Apply ops of higher presidence
            let presidence = get_presidence(op);
            while ops.last().is_some() && get_presidence(ops.last().unwrap()) > presidence {
                let a = exprs.pop().unwrap();
                let b = exprs.pop().unwrap();
                let op = ops.pop().unwrap();
                let e = apply_op(&op, b, a);
                exprs.push(e);
            }
            ops.push(*op);
        }
    }

    // Apply remaining ops in order
    dbg!("Remaining ops", &exprs, &ops);
    while ops.last().is_some() {
        let a = exprs.pop().unwrap();
        let b = exprs.pop().unwrap();
        let op = ops.pop().unwrap();
        let e = apply_op(&op, b, a);
        exprs.push(e);
    }
    dbg!("Applied remaining ops", &exprs, &ops);

    match exprs.pop() {
        Some(e) => Ok(e),
        None => Err(ParseError::other(format!("Incomplete expression {:?}", &tokens[*start..])))
    }
}

#[cfg(test)]
mod tests {
    use crate::parsing::{
        lexing,
        parsing::{AstNode, BinOperation},
    };

    use super::consume_expression;

    #[test]
    fn shunting_yard_1() {
        let toks = lexing::lex(&"1 + 2 * 4 + 8 * 7;");
        let ast = consume_expression(&toks, &mut 0).unwrap();
        dbg!(&ast);

        let AstNode::BinOp { op: BinOperation::Add, lhs, rhs} = ast.as_ref() else {
            panic!();
        };

        let AstNode::LiteralNumber(1) = lhs.as_ref() else {
            panic!();
        };

        let AstNode::BinOp { op: BinOperation::Add, lhs, rhs } = rhs.as_ref() else {
            panic!();
        };

        let AstNode::BinOp { op: BinOperation::Mult, lhs: lhs1, rhs:rhs1} = lhs.as_ref() else {
            panic!();
        };

        let AstNode::LiteralNumber(2) = lhs1.as_ref() else {
            panic!();
        };

        let AstNode::LiteralNumber(4) = rhs1.as_ref() else {
            panic!();
        };

        let AstNode::BinOp { op: BinOperation::Mult, lhs: lhs2, rhs: rhs2} = rhs.as_ref() else {
            panic!();
        };

        let AstNode::LiteralNumber(8) = lhs2.as_ref() else {
            panic!();
        };

        let AstNode::LiteralNumber(7) = rhs2.as_ref() else {
            panic!();
        };
    }

    #[test]
    fn shunting_yard_2() {
        let toks = lexing::lex(&"a = (1 + 2)() + square() * 4;");
        let ast = consume_expression(&toks, &mut 0).unwrap();
        dbg!(&ast);
    }
}
