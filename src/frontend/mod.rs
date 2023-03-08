mod fn_context;
mod function;

use function::Statement;
use std::{
    collections::HashMap,
    fmt::{self, Debug, Formatter},
};
use crate::{context::Context, error::LatteError, ast::Def};

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum FunctionId<'a> {
    Global { name: &'a str },
    Method { name: &'a str, class: &'a str },
}

impl<'a> Debug for FunctionId<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Global { name } => f.write_str(name),
            Self::Method { name, class } => write!(f, "{}::{}", class, name),
        }
    }
}

pub struct CheckedProgram<'a> {
    ctx: Context<'a>,
    defs: HashMap<FunctionId<'a>, Vec<Statement<'a>>>,
}

impl<'a> CheckedProgram<'a> {
    pub fn new(defs: Vec<Def<'a>>) -> Result<(), LatteError> {
        let context = Context::new(&defs)?;
        todo!()
    }

    pub fn ctx(&self) -> &Context<'a> {
        &self.ctx
    }

    pub fn defs(&self) -> &HashMap<FunctionId<'a>, Vec<Statement<'a>>> {
        &self.defs
    }
}

impl<'a> From<CheckedProgram<'a>> for Context<'a> {
    fn from(value: CheckedProgram<'a>) -> Self {
        value.ctx
    }
}
