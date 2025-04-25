use std::cell::RefCell;
use std::ops::Deref;
use std::ops::DerefMut;

// Values act as operator presidence
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOperation {
    Assign,
    Call,
    Bang,
    Mult,
    Div,
    Add,
    Sub,
    And,
    Or,
    Eq,
    Lt,
    Gt,
    LEq,
    GEq,
    NEq,
    LiteralJoinTuple,
}

#[derive(Clone, Debug)]
pub struct FnParameter<'a> {
    name: &'a str,
    ty: Option<&'a str>,
}

#[derive(Clone)]
pub enum AstNode<'a> {
    FunctionDecl {
        identifier: &'a str,
        args: Vec<FnParameter<'a>>,
        ret_ty: Option<&'a str>,
        body: BasedAstNode<'a>,
    },
    LiteralNumber(i32),
    LiteralBool(bool),
    Not(BasedAstNode<'a>),
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
        /// Must be a tuple
        args_tuple: BasedAstNode<'a>,
    },
    Variable {
        identifier: String,
    },
    LiteralTuple {
        elements: Vec<BasedAstNode<'a>>,
    },
    EmptyParens,
    Return {
        value: Option<BasedAstNode<'a>>,
    },
    IfStmt {
        condition: BasedAstNode<'a>,
        then_blk: BasedAstNode<'a>,
        else_blk: Option<BasedAstNode<'a>>,
    },
    WhileStmt {
        // Begin blk is always a Block
        begin_blk: BasedAstNode<'a>,
        condition: BasedAstNode<'a>,
        body_blk: BasedAstNode<'a>,
    },
}

impl std::fmt::Debug for AstNode<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_pretty(0, f)
    }
}

impl AstNode<'_> {
    /// The first line of output always is printed without adding indent. Multiline
    /// structures add indent to all subsequent lines.
    fn fmt_pretty(&self, indent: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LiteralBool(v) => f.write_str(&format!("{}", v)),
            Self::Not(expr) => {
                f.write_str("!")?;
                expr.as_ref().fmt_pretty(indent + 1, f)
            }
            Self::FunctionDecl {
                identifier,
                args,
                ret_ty,
                body,
            } => {
                f.write_str(&"  ".repeat(indent))?;
                f.write_str(&format!("fn {}({:?}) -> {:?}", identifier, args, ret_ty))?;
                body.as_ref().fmt_pretty(indent + 1, f)
            }
            Self::LiteralNumber(arg0) => f.write_str(&format!("{}", arg0)),
            Self::BinOp { op, lhs, rhs } => {
                f.write_str("(")?;
                lhs.as_ref().fmt_pretty(indent, f)?;
                println!("{:?} {:?}, {:?}", lhs, rhs, op);
                f.write_str(&format!(" {:?} ", op))?;
                rhs.as_ref().fmt_pretty(indent, f)?;
                f.write_str(")")
            }
            Self::Block { stmts } => {
                f.write_str("{\n")?;
                for stmt in stmts {
                    f.write_str(&"  ".repeat(indent + 1))?;
                    stmt.as_ref().fmt_pretty(indent + 1, f)?;
                    f.write_str("\n")?;
                }

                f.write_str(&"  ".repeat(indent))?;
                f.write_str("}")?;
                Ok(())
            }
            Self::Declaration { identifier, rhs } => {
                f.write_str(&format!("let {} = {:?}", identifier, rhs))
            }
            Self::Assignment { lhs, rhs } => f.write_str(&format!("{:?} = {:?}", lhs, rhs)),
            Self::FunctionCall {
                function,
                args_tuple: args,
            } => f.write_str(&format!("{:?}({:?})", function, args)),
            Self::Variable { identifier } => f.write_str(identifier),
            Self::LiteralTuple { elements } => f.write_str(&format!("{:?}", elements)),
            Self::IfStmt {
                condition,
                then_blk,
                else_blk,
            } => {
                f.write_str("if ")?;
                condition.as_ref().fmt_pretty(indent, f)?;
                f.write_str(" ")?;
                then_blk.as_ref().fmt_pretty(indent, f)?;
                if let Some(else_blk) = else_blk {
                    f.write_str(" else ")?;
                    else_blk.as_ref().fmt_pretty(indent, f)?;
                }
                Ok(())
            }
            Self::EmptyParens => write!(f, "EmptyParens"),
            Self::Return { value } => f.debug_struct("Return").field("value", value).finish(),
            Self::WhileStmt {
                begin_blk,
                condition,
                body_blk,
            } => {
                f.write_str("while ")?;
                condition.as_ref().fmt_pretty(indent, f)?;
                f.write_str(" ")?;
                body_blk.as_ref().fmt_pretty(indent, f)?;
                Ok(())
            }
        }
    }
    pub fn child_nodes(&self) -> Vec<BasedAstNode> {
        match self {
            Not(expr) => {
                vec![expr.clone()]
            }
            LiteralBool(_) => vec![],
            FunctionDecl {
                identifier, body, ..
            } => {
                let mut res = vec![body.clone()];
                res.append(&mut body.child_nodes());
                res
            }
            LiteralNumber(_) => vec![],
            BinOp { op, lhs, rhs } => {
                let mut res = lhs.child_nodes();
                res.append(&mut rhs.child_nodes());
                res
            }
            Block { stmts } => {
                let mut res = vec![];
                for stmt in stmts {
                    res.push(stmt.clone());
                    res.append(&mut stmt.child_nodes());
                }
                res
            }
            Declaration { identifier, rhs } => rhs.child_nodes(),
            Assignment { lhs, rhs } => {
                let mut res = vec![lhs.clone(), rhs.clone()];
                res.append(&mut lhs.child_nodes());
                res.append(&mut rhs.child_nodes());
                res
            }
            FunctionCall {
                function,
                args_tuple: args,
            } => {
                let mut res = vec![function.clone()];
                res.append(&mut function.child_nodes());
                res.extend(args.child_nodes());
                res
            }
            Variable { identifier } => vec![],
            LiteralTuple { elements } => {
                let mut res = elements.clone();
                for el in elements {
                    res.append(&mut el.child_nodes());
                }
                res
            }
            EmptyParens => vec![],
            Return { value } => {
                if let Some(value) = value {
                    let mut res = vec![value.clone()];
                    res.append(&mut value.child_nodes());
                    res
                } else {
                    vec![]
                }
            }
            IfStmt {
                condition,
                then_blk,
                else_blk,
            } => {
                let mut res = vec![condition.clone(), then_blk.clone()];
                res.append(&mut condition.child_nodes());
                res.append(&mut then_blk.child_nodes());
                if let Some(else_blk) = else_blk {
                    res.push(else_blk.clone());
                    res.append(&mut else_blk.child_nodes());
                }
                res
            }
            WhileStmt {
                begin_blk,
                condition,
                body_blk,
            } => {
                let mut res = vec![begin_blk.clone(), condition.clone(), body_blk.clone()];
                res.append(&mut condition.child_nodes());
                res.append(&mut begin_blk.child_nodes());
                res.append(&mut body_blk.child_nodes());
                res
            }
        }
    }
}

use AstNode::*;

use super::lexing::Token;
use super::lexing::TokenType;

/// Also keeps a reference to the source token.
#[derive(Clone)]
pub struct BasedAstNode<'a> {
    inner: Box<AstNode<'a>>,
    token: Option<&'a Token>,
}

impl std::fmt::Debug for BasedAstNode<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(format!("{:?}", &self.inner).as_str())
    }
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

impl DerefMut for BasedAstNode<'_> {
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
    fn other(context: impl Into<String>) -> Self {
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
    mut err_cb: ErrConsumer,
) -> Vec<BasedAstNode<'a>>
where
    ErrConsumer: FnMut(ParseError),
{
    let mut i = 0 as usize;

    let mut result = Vec::<BasedAstNode<'a>>::new();

    let mut ctx = Context::new();

    while i < tokens.len() {
        let token = &tokens[i];
        i += 1;
        match token.ty {
            TokenType::Fn => {
                i -= 1; // Let consume_function consume fn
                match consume_function(&mut ctx, tokens, &mut i) {
                    Err(e) => {
                        err_cb(e);
                        return result;
                    }
                    Ok(node) => result.push(node),
                }
            }
            TokenType::LineComment => {}
            tok => {
                err_cb(ParseError::unexpected(token, "File top level", None));
                // TODO: Don't give up after the first problem, recover somehow.
                return result;
            }
        }
    }

    result
}

#[derive(Debug)]
struct Context {
    stack: Vec<String>,
    print_changes: bool,
}

impl Context {
    fn new() -> Self {
        Self {
            stack: vec![],
            print_changes: false,
        }
    }
    fn push(&mut self, a: impl Into<String>) {
        let s = a.into();
        if self.print_changes {
            println!("Pushed {}", s);
        }
        self.stack.push(s)
    }

    fn pop(&mut self) {
        let prev = self.stack.pop().unwrap();
        if self.print_changes {
            println!("Popped {}, top now {:?}", prev, self.stack.first())
        }
    }
}

/// start: The first token to consume, usually fn
fn consume_function<'a>(
    ctx: &mut Context,
    tokens: &'a [Token],
    next: &mut usize,
) -> PResult<BasedAstNode<'a>> {
    let Some(fn_tok) = tokens.get(*next) else {
        return Err(ParseError::missing("Expected fn"));
    };
    assert_eq!(fn_tok.ty, TokenType::Fn);
    *next += 1;

    let Some(ident_tok) = tokens.get(*next) else {
        return Err(ParseError::missing("Expected function name"));
    };
    *next += 1;

    let Some(l_paren) = tokens.get(*next) else {
        return Err(ParseError::missing("Expected lparen"));
    };
    *next += 1;

    let args = consume_function_params_list(ctx, tokens, next)?;

    let Some(r_paren) = tokens.get(*next) else {
        return Err(ParseError::missing("Expected lparen"));
    };
    *next += 1;

    let ret_ty = consume_function_ret_type(ctx, tokens, next)?;

    let Some(l_brace) = tokens.get(*next) else {
        return Err(ParseError::missing("Expected lbrace"));
    };
    *next += 1;

    *next -= 1;
    let blk = consume_block(ctx, tokens, next)?;

    return Ok(BasedAstNode::<'a> {
        inner: Box::new(FunctionDecl {
            identifier: &ident_tok.src,
            args,
            ret_ty,
            body: blk,
        }),
        token: Some(fn_tok),
    });
}

// Does not consume the closing paren,
// Is given the first token after the opening paren
fn consume_function_params_list<'a>(
    ctx: &mut Context,
    tokens: &'a [Token],
    start: &mut usize,
) -> PResult<Vec<FnParameter<'a>>> {
    enum State {
        ParamName,
        TypeOrComma,
        Type,
        Comma,
    }
    use State::*;
    let mut state = ParamName;
    let mut params: Vec<FnParameter> = vec![];

    while let Some(tok) = tokens.get(*start) {
        if tok.ty == TokenType::RParen {
            return Ok(params);
        };
        match state {
            ParamName => {
                if tok.ty != TokenType::Identifier {
                    return Err(ParseError::other("Invalid params list"));
                }
                params.push(FnParameter {
                    name: &tok.src,
                    ty: None,
                });
                state = TypeOrComma;
                *start += 1;
            }
            TypeOrComma => {
                if tok.ty == TokenType::Colon {
                    state = Type;
                } else {
                    state = ParamName;
                }
                *start += 1;
            }
            Type => {
                if tok.ty != TokenType::Identifier {
                    return Err(ParseError::other("Types must be lexically identifiers."));
                }
                params.last_mut().unwrap().ty = Some(&tok.src);
                state = Comma;
                *start += 1;
            }
            Comma => {
                if tok.ty != TokenType::Comma {
                    return Err(ParseError::missing("Expecting a comma"));
                }
                state = ParamName;
                *start += 1;
            }
        }
    }

    panic!("The above code is a state machine which should return in all cases");
}

/// Starts with token after )
fn consume_function_ret_type<'a>(
    ctx: &mut Context,
    tokens: &'a [Token],
    next: &mut usize,
) -> PResult<Option<&'a str>> {
    let Some(tok) = tokens.get(*next) else {
        return Ok(None);
    };

    if tok.ty != TokenType::Arrow {
        return Ok(None);
    }
    let Some(type_tok) = tokens.get(*next + 1) else {
        return Ok(None);
    };

    if type_tok.ty != TokenType::Identifier {
        return Err(ParseError::unexpected(
            type_tok,
            "Expecting type name identifier",
            Some(TokenType::Identifier),
        ));
    }
    *next += 2;

    return Ok(Some(type_tok.src.as_str()));
}

/// start should be the first token to consume, probably {
fn consume_block<'a>(
    ctx: &mut Context,
    tokens: &'a [Token],
    start: &mut usize,
) -> PResult<BasedAstNode<'a>> {
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
                ctx.push("If condition");
                let expr = consume_expression(ctx, tokens, *start.borrow_mut())?;
                ctx.pop();
                ctx.push("Then blk");
                let then_blk = consume_block(ctx, tokens, *start.borrow_mut())?;
                ctx.pop();
                let mut else_blk = None;
                if expect(TokenType::Else).is_ok() {
                    ctx.push("Else blk");
                    else_blk = Some(consume_block(ctx, tokens, *start.borrow_mut())?);
                    ctx.pop();
                }

                stmts.push(
                    IfStmt {
                        condition: expr,
                        then_blk,
                        else_blk,
                    }
                    .into(),
                )
            }
            TokenType::While => {
                ctx.push("While condition");
                let expr = consume_expression(ctx, tokens, *start.borrow_mut())?;
                ctx.pop();
                let begin_blk = AstNode::Block { stmts: vec![] }.into();
                ctx.push("Body blk");
                let body_blk = consume_block(ctx, tokens, *start.borrow_mut())?;
                ctx.pop();

                stmts.push(
                    WhileStmt {
                        begin_blk,
                        condition: expr,
                        body_blk,
                    }
                    .into(),
                )
            }
            TokenType::LBrace => {
                **start.borrow_mut() -= 1;
                ctx.push("Block");
                stmts.push(consume_block(ctx, tokens, *start.borrow_mut())?);
                ctx.pop();
            }
            TokenType::Let => {
                **start.borrow_mut() -= 1;
                let decl_node = consume_declaration(ctx, tokens, *start.borrow_mut())?;
                stmts.push(decl_node);
            }
            TokenType::Identifier => {
                **start.borrow_mut() -= 1;
                let expr_node = consume_expression(ctx, tokens, *start.borrow_mut())?;
                stmts.push(expr_node);
            }
            TokenType::LineComment => {}
            TokenType::RBrace => break,
            TokenType::Return => {
                let val = match take() {
                    Err(_) => None,
                    Ok(token) => match token.ty {
                        TokenType::Semi | TokenType::RBrace => None,
                        _ => {
                            **start.borrow_mut() -= 1;
                            Some(consume_expression(ctx, tokens, *start.borrow_mut())?)
                        }
                    },
                };
                stmts.push(BasedAstNode {
                    inner: Box::new(Return { value: val }),
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

fn consume_declaration<'a>(
    ctx: &mut Context,
    tokens: &'a [Token],
    start: &mut usize,
) -> PResult<BasedAstNode<'a>> {
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
    let rhs = consume_expression(ctx, tokens, *start.borrow_mut())?;

    Ok(BasedAstNode::<'a> {
        inner: Box::new(Declaration {
            identifier: &ident_tok.src,
            rhs,
        }),
        token: Some(ident_tok),
    })
}

/// Expressions are only ended by semicolons or ) or { or }
/// This function accepts the position of the first token of the expression
/// and returns the position of the first token not consumed. This does
/// not include the semi or ), unless the expression started on a (.
fn consume_expression<'a>(
    ctx: &mut Context,
    tokens: &'a [Token],
    start: &mut usize,
) -> PResult<BasedAstNode<'a>> {
    ctx.stack.push("Expresssion".to_string());
    let iter = tokens[*start..].iter().peekable();

    // TODO: Raise if given an operator before a value

    if tokens[*start].ty == TokenType::LParen {
        // return consume_expression
    }

    fn get_presidence(op: &BinOperation) -> u32 {
        match op {
            BinOperation::Call => 10,
            BinOperation::Bang => 9,
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
            BinOperation::And => 3,
            BinOperation::Or => 3,
            BinOperation::LiteralJoinTuple => 2,
            BinOperation::Assign => 1,
        }
    }

    fn apply_unary<'a>(op: &BinOperation, expr: BasedAstNode<'a>) -> BasedAstNode<'a> {
        match op {
            BinOperation::Bang => Not(expr).into(),
            x => panic!("Bang is the only unary operator. Not {:?}", x),
        }
    }

    fn apply_op<'a>(
        op: &BinOperation,
        lhs: BasedAstNode<'a>,
        rhs: BasedAstNode<'a>,
    ) -> BasedAstNode<'a> {
        match op {
            BinOperation::Bang => panic!("Bang is not a binary operation."),
            BinOperation::LiteralJoinTuple => {
                let mut els = Vec::<BasedAstNode<'a>>::new();
                match lhs.as_ref() {
                    LiteralTuple { elements } => els.extend(elements.clone()),
                    _ => els.push(lhs),
                };
                LiteralTuple { elements: els }.into()
            }
            BinOperation::And => BinOp {
                op: BinOperation::And,
                lhs,
                rhs,
            }
            .into(),
            BinOperation::Or => BinOp {
                op: BinOperation::Or,
                lhs,
                rhs,
            }
            .into(),
            BinOperation::Call => {
                let mut rhs = rhs;
                if let LiteralTuple { .. } = rhs.as_ref() {
                } else {
                    rhs = LiteralTuple {
                        elements: vec![rhs],
                    }
                    .into();
                };
                FunctionCall {
                    function: lhs,
                    args_tuple: rhs,
                }
                .into()
            }

            BinOperation::Assign => BasedAstNode {
                inner: Box::new(Assignment { lhs, rhs }),
                token: None,
            },
            BinOperation::Mult => BinOp {
                op: BinOperation::Mult,
                lhs,
                rhs,
            }
            .into(),
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
            BinOperation::Eq
            | BinOperation::NEq
            | BinOperation::Lt
            | BinOperation::Gt
            | BinOperation::LEq
            | BinOperation::GEq => BinOp { op: *op, lhs, rhs }.into(),
        }
    }

    // Presidence climbing
    let mut exprs = Vec::<BasedAstNode>::new();
    let mut ops = Vec::<BinOperation>::new();
    let mut last_token_was_op = false;
    let mut is_first_token = true;

    while *start < tokens.len() {
        let tok = &tokens[*start];
        if ctx.print_changes {
            println!("Take {:?}", tok);
            println!("Stack: {:?}", ctx.stack);
            println!("Exprs: {:?}", exprs);
            println!("Ops: {:?}", ops);
        }
        *start += 1;
        let op: Option<BinOperation> = match tok.ty {
            TokenType::Semi => break,
            TokenType::LParen => {
                let parenthized_expr = if tokens[*start].ty == TokenType::RParen {
                    Some(BasedAstNode {
                        inner: Box::new(EmptyParens),
                        token: Some(&tokens[*start]),
                    })
                } else {
                    let r = consume_expression(ctx, tokens, start)?;
                    Some(r)
                };

                let TokenType::RParen = tokens[*start].ty else {
                    return Err(ParseError::missing("Expression, expecting )"));
                };
                *start += 1;

                if let Some(expr) = parenthized_expr {
                    exprs.push(expr);
                }

                if !is_first_token
                    && (!last_token_was_op || ops.last().is_some_and(|x| *x == BinOperation::Call))
                {
                    Some(BinOperation::Call)
                } else {
                    None
                }
            }
            TokenType::LBrace | TokenType::RBrace => {
                *start -= 1;
                break;
            }
            TokenType::RParen => {
                *start -= 1;
                break;
            }
            TokenType::EqAssign => Some(BinOperation::Assign),
            TokenType::OpAdd => Some(BinOperation::Add),
            TokenType::OpSub => Some(BinOperation::Sub),
            TokenType::OpMult => Some(BinOperation::Mult),
            TokenType::True => {
                exprs.push(LiteralBool(true).into());
                None
            }
            TokenType::False => {
                exprs.push(LiteralBool(false).into());
                None
            }
            TokenType::Identifier => {
                exprs.push(BasedAstNode {
                    inner: Box::new(Variable {
                        identifier: tok.src.clone(),
                    }),
                    token: Some(tok),
                });
                None
            }
            TokenType::NumberLit => {
                exprs.push(BasedAstNode {
                    inner: Box::new(LiteralNumber(str::parse::<i32>(&tok.src).unwrap())),
                    token: Some(tok),
                });
                if exprs
                    .last()
                    .is_some_and(|it| matches!(*it.inner, AstNode::Variable { .. }))
                {
                    // Then it's a function call baby
                    Some(BinOperation::Call)
                } else {
                    None
                }
            }
            TokenType::And => Some(BinOperation::And),
            TokenType::Or => Some(BinOperation::Or),
            TokenType::EqCmp => Some(BinOperation::Eq),
            TokenType::NEqCmp => Some(BinOperation::NEq),
            TokenType::LCmp => Some(BinOperation::Lt),
            TokenType::GCmp => Some(BinOperation::Gt),
            TokenType::GEqCmp => Some(BinOperation::GEq),
            TokenType::LEqCmp => Some(BinOperation::LEq),
            TokenType::Bang => Some(BinOperation::Bang),
            TokenType::Comma => Some(BinOperation::LiteralJoinTuple),
            other => panic!("Expression encountered {:?}", other),
        };
        last_token_was_op = op.is_some();
        is_first_token = false;
        if let Some(ref op) = op {
            // Apply ops of higher presidence
            if ctx.print_changes {
                println!("Collapsing previous lower ops: {:?}", exprs);
            }
            let presidence = get_presidence(op);
            while ops.last().is_some() && get_presidence(ops.last().unwrap()) > presidence {
                let op = ops.pop().unwrap();
                if ctx.print_changes {
                    println!("Applying op {:?}", op);
                }

                let a = exprs.pop().unwrap();
                let e = if op == BinOperation::Bang {
                    apply_unary(&op, a)
                } else {
                    let b = exprs.pop().unwrap();
                    apply_op(&op, b, a)
                };
                exprs.push(e);
            }
            ops.push(*op);
            if ctx.print_changes {
                println!("Expr stack is now {:?}", exprs);
            }
        }
    }

    // Apply remaining ops in order
    while let Some(op) = ops.pop() {
        let a = exprs.pop().unwrap();
        let e = if op == BinOperation::Bang {
            apply_unary(&op, a)
        } else {
            let b = exprs.pop().unwrap();
            apply_op(&op, b, a)
        };
        exprs.push(e);
    }

    ctx.stack.pop();
    match exprs.pop() {
        Some(e) => Ok(e),
        None => Err(ParseError::other(format!(
            "Incomplete expression {:?}",
            &tokens[*start..]
        ))),
    }
}

#[cfg(test)]
mod tests {
    use crate::parsing::{
        lexing,
        parsing::{consume_block, consume_function, AstNode, BinOperation, Context},
    };

    use super::consume_expression;
    use super::parse_file;

    #[test]
    fn shunting_yard_1() {
        let toks = lexing::lex(&"1 + 2 * 4 + 8 * 7;");
        let ast = consume_expression(&mut Context::new(), &toks, &mut 0).unwrap();
        dbg!(&ast);

        let AstNode::BinOp {
            op: BinOperation::Add,
            lhs,
            rhs,
        } = ast.as_ref()
        else {
            panic!();
        };

        let AstNode::LiteralNumber(1) = lhs.as_ref() else {
            panic!();
        };

        let AstNode::BinOp {
            op: BinOperation::Add,
            lhs,
            rhs,
        } = rhs.as_ref()
        else {
            panic!();
        };

        let AstNode::BinOp {
            op: BinOperation::Mult,
            lhs: lhs1,
            rhs: rhs1,
        } = lhs.as_ref()
        else {
            panic!();
        };

        let AstNode::LiteralNumber(2) = lhs1.as_ref() else {
            panic!();
        };

        let AstNode::LiteralNumber(4) = rhs1.as_ref() else {
            panic!();
        };

        let AstNode::BinOp {
            op: BinOperation::Mult,
            lhs: lhs2,
            rhs: rhs2,
        } = rhs.as_ref()
        else {
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
        let ast = consume_expression(&mut Context::new(), &toks, &mut 0).unwrap();
        dbg!(&ast);
    }

    #[test]
    fn if_blocks() {
        use AstNode::*;

        let toks = lexing::lex(&"{if 1 {print(a)} else {print(b)}}");
        let mut ctx = Context::new();
        ctx.print_changes = true;
        let ast = consume_block(&mut ctx, &toks, &mut 0).unwrap();
        let Block { stmts } = ast.as_ref() else {
            panic!();
        };
        let IfStmt {
            condition,
            then_blk,
            else_blk,
        } = &*stmts[0].inner
        else {
            panic!();
        };
        let Block { stmts } = then_blk.as_ref() else {
            panic!();
        };
        let FunctionCall {
            function,
            args_tuple: args,
        } = stmts[0].as_ref()
        else {
            panic!();
        };
        let Block { stmts } = else_blk.as_ref().unwrap().as_ref() else {
            panic!();
        };
        let FunctionCall {
            function,
            args_tuple: args,
        } = stmts[0].as_ref()
        else {
            panic!();
        };

        dbg!(&ast);
    }

    #[test]
    fn bool_compound_expr() {
        let toks = lexing::lex(&"print((5+6) == (7+8));");
        let mut ctx = Context::new();
        ctx.print_changes = true;
        let ast = consume_expression(&mut ctx, &toks, &mut 0).unwrap();
    }

    #[test]
    fn bool_literals_and_bang() {
        use AstNode::*;
        let toks = lexing::lex(&"!true == false");
        let mut ctx = Context::new();
        ctx.print_changes = true;
        let ast = consume_expression(&mut ctx, &toks, &mut 0).unwrap();

        let BinOp { op, lhs, rhs } = ast.as_ref() else {
            panic!();
        };
        let Not(llhs) = lhs.as_ref() else {
            panic!();
        };
        let LiteralBool(true) = llhs.as_ref() else {
            panic!()
        };
        let LiteralBool(false) = rhs.as_ref() else {
            panic!()
        };

        assert_eq!(BinOperation::Eq, *op);
    }

    #[test]
    fn and_or() {
        use AstNode::*;
        let toks = lexing::lex(&"true and false or false");
        let mut ctx = Context::new();
        ctx.print_changes = true;
        let ast = consume_expression(&mut ctx, &toks, &mut 0).unwrap();

        let BinOp { op, lhs, rhs } = ast.as_ref() else {
            panic!();
        };
        assert_eq!(*op, BinOperation::And);

        let BinOp { op, lhs, rhs } = rhs.as_ref() else {
            panic!();
        };
        assert_eq!(*op, BinOperation::Or);
    }

    #[test]
    fn a4_4() {
        let toks = lexing::lex(
            &r#"
// {"compiles": true, "stdout": "9\n"}

fn main() {
    // x = false

    let x = ((5 == 6) and ((6+7) > 3) or (5 < 3));

    if !x {
        print(9);
    } else {
        print(10);
    }
}
"#,
        );
        let _ = parse_file(&toks, |err| panic!("Parse error: {:?}", err));
    }

    #[test]
    fn a5_1() {
        use AstNode::*;
        let toks = lexing::lex(
            &r#"
{
	let i = 1;
	while i < 10 {
		i = i + 1;
	}
}
"#,
        );

        let mut ctx = Context::new();
        ctx.print_changes = true;
        let ast = consume_block(&mut ctx, &toks, &mut 0).unwrap();
        dbg!(&ast);

        let Block { stmts } = ast.as_ref() else {
            panic!();
        };

        let WhileStmt {
            begin_blk,
            condition,
            body_blk,
        } = stmts[1].as_ref()
        else {
            panic!();
        };
        let Block { stmts } = begin_blk.as_ref() else {
            panic!();
        };
        assert_eq!(stmts.len(), 0);

        let BinOp { op, lhs, rhs } = condition.as_ref() else {
            panic!()
        };

        let Block { stmts } = body_blk.as_ref() else {
            panic!()
        };
    }

    #[test]
    fn function_signature_1() {
        let toks = lexing::lex(
            &r#"
fn add1(n: int) -> int {
    return n + 1;
}
"#,
        );
        let mut ctx = Context::new();
        ctx.print_changes = true;
        let res = consume_function(&mut ctx, &toks, &mut 0);
        if let Err(parse_err) = res {
            panic!("{:?}", parse_err);
        };
        let ast = res.unwrap();
        dbg!(&ast);
    }

    #[test]
    fn function_signature_2() {
        let toks = lexing::lex(
            &r#"
fn add1(n: int, a_tw: str) -> Cat {
    return n + 1;
}
"#,
        );
        let mut ctx = Context::new();
        ctx.print_changes = true;
        let res = consume_function(&mut ctx, &toks, &mut 0);
        if let Err(parse_err) = res {
            panic!("{:?}", parse_err);
        };
        let ast = res.unwrap();
        dbg!(&ast);
    }

    #[test]
    fn function_call() {
        let toks = lexing::lex(
            &r#"
{
    add(a, 1);   
    take(a);    
    take a;    
    add((a, 1));
}
"#,
        );
        // call(add, tuple(a, 1))
        // call(add, tuple(a))
        // call(add, tuple(a))
        // call(add, tuple(tuple(a, 1)))
        let mut ctx = Context::new();
        ctx.print_changes = true;
        let res = consume_block(&mut ctx, &toks, &mut 0);
        if let Err(parse_err) = res {
            panic!("{:?}", parse_err);
        };
        let ast = res.unwrap();
        dbg!(&ast);

        let AstNode::Block { stmts } = ast.as_ref() else {
            panic!()
        };
        let AstNode::FunctionCall {
            function,
            args_tuple,
        } = stmts[0].as_ref()
        else {
            panic!()
        };
        let AstNode::LiteralTuple { elements } = args_tuple.as_ref() else {
            panic!()
        };

        let AstNode::FunctionCall {
            function,
            args_tuple,
        } = stmts[1].as_ref()
        else {
            panic!()
        };
        let AstNode::LiteralTuple { elements } = args_tuple.as_ref() else {
            panic!()
        };

        dbg!(&stmts[2]);
        let AstNode::FunctionCall {
            function,
            args_tuple,
        } = stmts[2].as_ref()
        else {
            panic!("{:?}", stmts[2])
        };
        let AstNode::LiteralTuple { elements } = args_tuple.as_ref() else {
            panic!()
        };
    }
}
