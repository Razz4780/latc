mod generator;
mod hir;

use crate::{
    context::Context,
    frontend::{CheckedProgram, FunctionId},
};
use generator::HirGenerator;
use std::collections::HashMap;

pub use generator::{HirFunction, HirLoc};
pub use hir::*;

pub struct HirProgram<'a> {
    context: Context<'a>,
    functions: HashMap<FunctionId<'a>, HirFunction<'a>>,
}

impl<'a> HirProgram<'a> {
    pub fn new(program: CheckedProgram<'a>) -> Self {
        let mut functions: HashMap<FunctionId<'a>, HirFunction<'a>> = Default::default();

        for (name, stmts) in program.defs() {
            let fun = match name {
                FunctionId::Global { name } => program.ctx().function(name).unwrap(),
                FunctionId::Method { name, class } => program
                    .ctx()
                    .class(class)
                    .unwrap()
                    .method(name)
                    .unwrap()
                    .as_fun(),
            };
            let mut generator = HirGenerator::new(fun, program.ctx());
            for stmt in stmts {
                generator.process_stmt(stmt);
            }
            let fun = generator.finish();

            functions.insert(*name, fun);
        }

        Self {
            context: program.into(),
            functions,
        }
    }

    pub fn context(&self) -> &Context<'a> {
        &self.context
    }

    pub fn functions(&self) -> &HashMap<FunctionId<'a>, HirFunction<'a>> {
        &self.functions
    }
}
