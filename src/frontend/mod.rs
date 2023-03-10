mod builder;
mod fn_context;
mod types;

use crate::{ast::Def, context::Context, error::StaticCheckError};
use builder::FunctionBuilder;
use std::{
    collections::HashMap,
    fmt::{self, Debug, Formatter},
};

pub use types::*;

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
    pub fn new(dirty: Vec<Def<'a>>) -> Result<Self, StaticCheckError> {
        let ctx = Context::new(&dirty)?;
        let mut defs = HashMap::new();

        for def in dirty {
            match def {
                Def::Fn(fn_def) => {
                    let id = FunctionId::Global {
                        name: fn_def.ident.inner,
                    };
                    let ctx = ctx.fn_context(&id).unwrap();
                    let stmts = fn_def.stmts;
                    let mut builder = FunctionBuilder::new(ctx);
                    for stmt in stmts {
                        builder.add_statement(stmt)?;
                    }
                    let stmts = builder.finish()?;
                    defs.insert(id, stmts);
                }
                Def::Class(class_def) => {
                    for fn_def in class_def.methods {
                        let id = FunctionId::Method {
                            name: fn_def.ident.inner,
                            class: class_def.ident.inner,
                        };
                        let ctx = ctx.fn_context(&id).unwrap();
                        let stmts = fn_def.stmts;
                        let mut builder = FunctionBuilder::new(ctx);
                        for stmt in stmts {
                            builder.add_statement(stmt)?;
                        }
                        let stmts = builder.finish()?;
                        defs.insert(id, stmts);
                    }
                }
            }
        }

        Ok(Self { ctx, defs })
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
