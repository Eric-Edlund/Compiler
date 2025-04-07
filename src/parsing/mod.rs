use lexing::Token;
use parsing::{BasedAstNode, ParseError};

pub mod lexing;
pub mod parsing;

pub struct FileAnal<'a> {
    pub asts: Vec<BasedAstNode<'a>>,
    pub src_tokens: Vec<Token>,
}


/// Build an ast for every top level structure in the file, in the order they appear.
pub fn build_ast<'a, F>(buf: impl AsRef<[u8]>, err_cb: F) -> FileAnal<'a> 
where F: FnMut(ParseError)
{
    // let mut buf = Vec::<u8>::with_capacity(f.metadata().unwrap().len() as usize);
    // let _ = f.read(&mut buf).unwrap();

    let tokens = Box::new(lexing::lex(&buf.as_ref()));
    let asts: Vec<BasedAstNode<'a>>;
    unsafe {
        let tokens_ptr: *const Vec<_> = &*tokens;
        asts = parsing::parse_file(&*tokens_ptr, err_cb);
    }

    FileAnal::<'a> {
        asts,
        src_tokens: *tokens,
    }
}
