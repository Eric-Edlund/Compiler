//! For now, don't assign registers, use all stack variables.
use super::common::{X86Instr, X86Program};



/// The program is currently in pseudo x86 because it has variables.
/// In this pass we remove variables, replacing them with either 
/// registers or stack references.
pub fn allocate_registers(program : &mut X86Program) {
    for block in &mut program.blocks {
        


    }
}


fn reads_of(instr: X86Instr) {
    use X86Instr::*;
    match instr {
        Addq { val, rd } => {

        }
        _ => todo!()

    }

}

fn writes_of(instr: X86Instr) {

}
