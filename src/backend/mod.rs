mod asm;
mod intervals;
mod regalloc;

use crate::middlend::HirProgram;
pub use asm::AsmProgram;

pub fn process(program: HirProgram<'_>) -> AsmProgram<'_> {
    AsmProgram::new(program)
}
