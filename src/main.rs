#![allow(
    clippy::needless_borrows_for_generic_args,
    clippy::needless_return,
    dead_code,
    clippy::unnecessary_cast,
    clippy::module_inception,
    unused_variables,
    clippy::needless_else,
    clippy::useless_format,
    clippy::needless_range_loop,
    clippy::redundant_field_names,
)]
mod ast_stuff;
mod type_stuff;
mod codegen_stuff;
mod parsing;

use ast_stuff::rco::remove_complex_operands;
use clap::Parser;
use codegen_stuff::allocate_registers::allocate_registers;
use codegen_stuff::render_x86::render;
use codegen_stuff::patch_instructions::patch_instructions;
use codegen_stuff::program_setup::{prelude_and_conclusion, wrap_functions_with_stack_logic};
use codegen_stuff::select_instructions::select_instructions;
use type_stuff::type_checking::type_check;
use core::str;
use std::process::exit;
use parsing::build_ast;
use std::fs::File;
use std::io::{Read, Write};
use std::path::PathBuf;

#[derive(Parser, Debug)]
struct Args {
    input_file: PathBuf,
    #[arg(short)]
    output_file: PathBuf,

    /// Print the intermedate results from compilation phases.
    #[arg(short)]
    debug: bool,

    /// Do not use registers in compiled code.
    #[arg(long)]
    no_registers: bool,
}

#[tokio::main]
async fn main() {
    let args = Args::parse();

    let if_debug = |msg: String| {
        if args.debug {
            println!("{}", msg)
        }
    };

    let mut root = File::open(&args.input_file).unwrap();
    let mut buf = Vec::<u8>::with_capacity(root.metadata().unwrap().len() as usize);
    root.read_to_end(&mut buf).unwrap();
    if_debug(format!("Program Text:\n\n{}\n", str::from_utf8(&buf).unwrap()));

    let mut parsed_file = build_ast(buf, |prb| {println!("Parse error: {:?}", prb); exit(1)});
    let tuple_vars = type_check(&parsed_file).expect("Failed type check.");
    if_debug(format!("Tuple Variables: {:?}", tuple_vars));
    if_debug(format!("Passed Type Check.\n"));

    if_debug(format!("Parsed AST:\n\n{:?}\n", parsed_file.asts));
    remove_complex_operands(&mut parsed_file);
    if_debug(format!("RCO:\n\n{:?}\n", parsed_file.asts));
    let tuple_vars = type_check(&parsed_file).expect("Failed type check.");
    if_debug(format!("Tuple Variables: {:?}\n", tuple_vars));

    // This pass preserves variable names
    let mut x86_program = select_instructions(&parsed_file);
    if_debug(format!("Select Instructions:\n\n{}\n", String::from_utf8(render(&x86_program)).unwrap()));
    allocate_registers(&mut x86_program, args.no_registers, &tuple_vars);
    wrap_functions_with_stack_logic(&mut x86_program);
    if_debug(format!("Allocate Registers:\n\n{}\n", String::from_utf8(render(&x86_program)).unwrap()));
    prelude_and_conclusion(&mut x86_program);

    if_debug(format!("Prelude & Conclusion:\n\n{}\n", String::from_utf8(render(&x86_program)).unwrap()));
    patch_instructions(&mut x86_program);
    let program_assembly = render(&x86_program);
    

    let mut out = File::create(&args.output_file).unwrap();
    out.write_all(&program_assembly).unwrap();
}
