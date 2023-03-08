mod asm;
mod intervals;
mod regalloc;

use crate::middlend::HirProgram;
use asm::AsmProgram;

pub fn process<'a>(program: HirProgram<'a>) -> AsmProgram<'a> {
    AsmProgram::new(program)
}
