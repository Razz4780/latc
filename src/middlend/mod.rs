mod context;
mod generator;
mod hir;
mod layout;
mod size;

pub use context::{Context, Function, Symbol};
pub use generator::{HirFunction, HirLoc};
pub use hir::{BinOp, BlockId, Hir, PhiOption, RelCond, VReg, VRegId, Value};
pub use size::{Bytes, GetSize};

use crate::frontend::{FunctionId, Program};
use generator::HirGenerator;
use std::collections::HashMap;

pub struct HirProgram<'a> {
    context: Context<'a>,
    functions: HashMap<FunctionId<'a>, HirFunction<'a>>,
}

impl<'a> HirProgram<'a> {
    pub fn context(&self) -> &Context<'a> {
        &self.context
    }

    pub fn functions(&self) -> &HashMap<FunctionId<'a>, HirFunction<'a>> {
        &self.functions
    }
}

pub fn process<'a>(program: Program<'a>) -> HirProgram<'a> {
    let context = Context::new(&program);
    let mut functions: HashMap<FunctionId<'a>, HirFunction<'a>> = Default::default();

    for (name, stmts) in program.definitions() {
        let fun = match name {
            FunctionId::Global { name } => context.function(name),
            FunctionId::Method { name, class } => context.class(class).method(name).as_fun(),
        };
        let mut generator = HirGenerator::new(fun, &context);
        for stmt in stmts {
            generator.process_stmt(stmt);
        }
        let fun = generator.finish();

        functions.insert(*name, fun);
    }

    HirProgram { context, functions }
}
