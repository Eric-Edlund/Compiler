#![allow(
    clippy::needless_borrows_for_generic_args,
    clippy::needless_return,
    dead_code,
    clippy::unnecessary_cast,
    clippy::module_inception,
    unused_variables,
    clippy::needless_else
)]

pub mod parsing;
pub mod ast_stuff;
pub mod type_stuff;
pub mod codegen_stuff;

use codegen_stuff::render_x86::render;
use codegen_stuff::patch_instructions::patch_instructions;
use parsing::build_ast;
use ast_stuff::explicate_control::explicate_control;
use ast_stuff::rco::remove_complex_operands;
use codegen_stuff::allocate_registers::allocate_registers;
use codegen_stuff::program_setup::{prelude_and_conclusion, wrap_functions_with_stack_logic};
use codegen_stuff::select_instructions::select_instructions;
use type_stuff::type_checking::type_check;
use core::str;

type CompileResult = Result<Vec<u8>, Vec<String>>;

#[derive(Debug)]
pub struct CompilationConfig {
    pub no_registers: bool
}

pub fn compile_program(src: &str, config: &CompilationConfig) -> CompileResult {
    let mut syntax_err = false;
    let mut parsed_file = build_ast(src, |_| {syntax_err = true;});
    if syntax_err {
        return Err(vec!["Syntax error.".to_string()]);
    }
    let type_res = type_check(&parsed_file);
    type_res?;

    remove_complex_operands(&mut parsed_file);
    explicate_control(&mut parsed_file);

    let mut x86_program = select_instructions(&parsed_file);
    allocate_registers(&mut x86_program, config.no_registers);
    // if_debug(format!("Allocate Registers:\n\n{}\n", String::from_utf8(render(&x86_program)).unwrap()));
    wrap_functions_with_stack_logic(&mut x86_program);
    prelude_and_conclusion(&mut x86_program);
    patch_instructions(&mut x86_program);

    let program_assembly = render(&x86_program);
    return Ok(program_assembly)
}
