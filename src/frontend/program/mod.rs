pub mod builder;
pub mod function;

use crate::frontend::declarations::Declarations;
use function::Statement;
use std::{
    collections::HashMap,
    fmt::{self, Debug, Formatter},
};

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

pub struct Program<'a> {
    declarations: Declarations<'a>,
    definitions: HashMap<FunctionId<'a>, Vec<Statement<'a>>>,
}

impl<'a> Program<'a> {
    pub fn declarations(&self) -> &Declarations<'a> {
        &self.declarations
    }

    pub fn definitions(&self) -> &HashMap<FunctionId<'a>, Vec<Statement<'a>>> {
        &self.definitions
    }
}
