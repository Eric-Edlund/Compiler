#![allow(
    clippy::needless_borrows_for_generic_args,
    clippy::needless_return,
    dead_code,
    clippy::unnecessary_cast,
    clippy::module_inception,
    unused_variables,
    clippy::needless_else
)]

use codegen_stuff::assembly::render;
use parsing::build_ast;
use ast_stuff::explicate_control::explicate_control;
use ast_stuff::rco::remove_complex_operands;
use codegen_stuff::allocate_registers::allocate_registers;
use codegen_stuff::program_setup::prelude_and_conclusion;
use codegen_stuff::select_instructions::select_instructions;
use codegen_stuff::type_checking::type_check;
use core::str;

pub mod parsing;
pub mod ast_stuff;
pub mod codegen_stuff;


type CompileResult = Result<Vec<u8>, Vec<String>>;

pub fn compile_program(src: &str) -> CompileResult {
    let mut parsed_file = build_ast(src, |_| {});
    let type_res = type_check(&parsed_file);
    type_res?;

    remove_complex_operands(&mut parsed_file);
    explicate_control(&mut parsed_file);

    let mut x86_program = select_instructions(&parsed_file);
    allocate_registers(&mut x86_program);
    // if_debug(format!("Allocate Registers:\n\n{}\n", String::from_utf8(render(&x86_program)).unwrap()));
    prelude_and_conclusion(&mut x86_program);

    let program_assembly = render(&x86_program);
    return Ok(program_assembly)
}
