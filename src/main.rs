#![allow(
    clippy::needless_borrows_for_generic_args,
    clippy::needless_return,
    dead_code,
    clippy::unnecessary_cast,
    clippy::module_inception,
    unused_variables,
    clippy::needless_else
)]
mod parsing;
mod ast_stuff;
mod codegen_stuff;

use codegen_stuff::allocate_registers::allocate_registers;
use codegen_stuff::select_instructions::select_instructions;
use clap::Parser;
use parsing::build_ast;
use core::str;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

#[derive(Parser, Debug)]
struct Args {
    input_file: PathBuf,
}

#[tokio::main]
async fn main() {
    let args = Args::parse();

    let mut root = File::open(&args.input_file).unwrap();
    let mut buf = Vec::<u8>::with_capacity(root.metadata().unwrap().len() as usize);
    _ = root.read_to_end(&mut buf);
    println!("{}", str::from_utf8(&buf).unwrap());

    let mut parsed_file = build_ast(buf, |_| {});
    ast_stuff::rco::remove_complex_operands(&mut parsed_file);
    println!("{:?}", parsed_file.asts);

    let mut x86_program = select_instructions(&parsed_file);
    allocate_registers(&mut x86_program);

    println!("{:?}", x86_program);
}
