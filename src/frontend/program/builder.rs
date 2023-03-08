use super::{
    function::{FunctionBuilder, Statement},
    FunctionId, Program,
};
use crate::{
    ast::Stmt,
    error::LatteError,
    frontend::declarations::{function::FnDecl, Declarations},
};
use std::{collections::HashMap, rc::Rc};

pub struct ProgramBuilder<'a> {
    declarations: Rc<Declarations<'a>>,
    definitions: HashMap<FunctionId<'a>, Vec<Statement<'a>>>,
}

impl<'a> ProgramBuilder<'a> {
    pub fn new(declarations: Declarations<'a>) -> Self {
        Self {
            declarations: Rc::new(declarations),
            definitions: Default::default(),
        }
    }

    pub fn add_function(
        &mut self,
        name: &'a str,
        statements: Vec<Stmt<'a>>,
    ) -> Result<(), LatteError> {
        let context = self
            .declarations
            .clone()
            .function_context(name)
            .ok_or_else(|| LatteError::new(format!("undefined function \"{}\"", name)))?;

        let mut builder = FunctionBuilder::new(context);
        for statement in statements {
            builder.add_statement(statement)?;
        }
        let definition = builder.finish()?;

        self.definitions
            .insert(FunctionId::Global { name }, definition);

        Ok(())
    }

    pub fn add_method(
        &mut self,
        name: &'a str,
        class: &'a str,
        statements: Vec<Stmt<'a>>,
    ) -> Result<(), LatteError> {
        let context = self
            .declarations
            .clone()
            .method_context(name, class)
            .ok_or_else(|| {
                LatteError::new(format!(
                    "undefined method \"{}\" in class \"{}\"",
                    name, class
                ))
            })?;

        let mut builder = FunctionBuilder::new(context);
        for statement in statements {
            builder.add_statement(statement)?;
        }
        let definition = builder.finish()?;

        self.definitions
            .insert(FunctionId::Method { name, class }, definition);

        Ok(())
    }

    pub fn build(self) -> Result<Program<'a>, LatteError> {
        let global_functions = self
            .declarations
            .functions()
            .keys()
            .copied()
            .filter(|name| !FnDecl::is_builtin(name));
        for name in global_functions {
            let id = FunctionId::Global { name };
            if !self.definitions.contains_key(&id) {
                return Err(LatteError::new(format!(
                    "no definition for declared global function \"{}\"",
                    name
                )));
            }
        }

        for (class, class_decl) in self.declarations.classes() {
            for (name, _) in class_decl.methods() {
                let id = FunctionId::Method { name, class };
                if !self.definitions.contains_key(&id) {
                    return Err(LatteError::new(format!(
                        "no definition for method \"{}\" declared in class \"{}\"",
                        name, class
                    )));
                }
            }
        }

        Ok(Program {
            declarations: Rc::try_unwrap(self.declarations)
                .expect("clones should not escape the add_* methods"),
            definitions: self.definitions,
        })
    }
}
