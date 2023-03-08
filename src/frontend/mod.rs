mod declarations;
mod program;

pub use self::{
    declarations::{class::ClassDecl, function::FnDecl, Declarations},
    program::{
        function::{Assignable, CallAddr, Expression, Statement},
        FunctionId, Program,
    },
};

use self::{declarations::builder::DeclarationsBuilder, program::builder::ProgramBuilder};
use crate::{ast::Def, error::LatteError};

pub fn process(defs: Vec<Def<'_>>) -> Result<Program<'_>, LatteError> {
    let declarations = {
        let mut declarations_builder = DeclarationsBuilder::default();
        for def in &defs {
            match def {
                Def::Fn(fun) => declarations_builder.add_function(fun)?,
                Def::Class(class) => declarations_builder.add_class(class)?,
            }
        }
        declarations_builder.build()?
    };

    let program = {
        let mut program_builder = ProgramBuilder::new(declarations);
        for def in defs {
            match def {
                Def::Fn(fun) => program_builder.add_function(fun.ident.inner, fun.stmts)?,
                Def::Class(class) => {
                    for method in class.methods {
                        program_builder.add_method(
                            method.ident.inner,
                            class.ident.inner,
                            method.stmts,
                        )?;
                    }
                }
            }
        }
        program_builder.build()?
    };

    Ok(program)
}
