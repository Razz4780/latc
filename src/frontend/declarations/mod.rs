pub mod builder;
pub mod class;
pub mod function;

use crate::ast::Type;
use class::ClassDecl;
use function::FnDecl;
use std::{collections::HashMap, rc::Rc};

#[derive(Debug)]
pub struct Declarations<'a> {
    functions: HashMap<&'a str, FnDecl<'a>>,
    classes: HashMap<&'a str, ClassDecl<'a>>,
}

impl<'a> Declarations<'a> {
    pub fn functions(&self) -> &HashMap<&'a str, FnDecl<'a>> {
        &self.functions
    }

    pub fn classes(&self) -> &HashMap<&'a str, ClassDecl<'a>> {
        &self.classes
    }

    pub fn function_context(self: Rc<Self>, name: &'a str) -> Option<Context<'a>> {
        if self.functions.contains_key(name) {
            Some(Context {
                declarations: self,
                class_name: None,
                name,
            })
        } else {
            None
        }
    }

    pub fn method_context(
        self: Rc<Self>,
        name: &'a str,
        class_name: &'a str,
    ) -> Option<Context<'a>> {
        if let Some(class_context) = self.classes.get(class_name) {
            if class_context.methods().iter().any(|m| m.0 == name) {
                return Some(Context {
                    declarations: self,
                    class_name: Some(class_name),
                    name,
                });
            }
        }

        None
    }
}

pub struct Context<'a> {
    declarations: Rc<Declarations<'a>>,
    class_name: Option<&'a str>,
    name: &'a str,
}

impl<'a> Context<'a> {
    pub fn name(&self) -> &'a str {
        self.name
    }

    pub fn class_name(&self) -> Option<&'a str> {
        self.class_name
    }

    fn function_context(&self) -> &FnDecl<'a> {
        match self.class_name {
            Some(class) => {
                &self
                    .declarations
                    .classes()
                    .get(class)
                    .expect("this class should exist")
                    .methods()
                    .iter()
                    .find(|m| m.0 == self.name)
                    .expect("this method should exist")
                    .1
            }
            None => self
                .declarations
                .functions()
                .get(self.name)
                .expect("this function should exist"),
        }
    }

    pub fn args(&self) -> &[(&'a str, Type<'a>)] {
        self.function_context().args()
    }

    pub fn ret(&self) -> Option<Type<'a>> {
        self.function_context().ret()
    }

    pub fn get_class(&self, class: &'a str) -> Option<&ClassDecl<'a>> {
        self.declarations.classes().get(class)
    }

    pub fn get_function(&self, name: &'a str) -> Option<&FnDecl<'a>> {
        self.declarations.functions().get(name)
    }

    pub fn get_field(&self, name: &'a str) -> Option<(&'a str, Type<'a>)> {
        self.get_field_of(self.class_name?, name)
    }

    pub fn get_field_of(&self, mut class: &'a str, name: &'a str) -> Option<(&'a str, Type<'a>)> {
        loop {
            let decl = self.declarations.classes().get(class)?;
            let res = decl.fields().iter().find(|f| f.0 == name);
            if let Some((_, t)) = res {
                return Some((class, *t));
            }

            class = decl.parent()?;
        }
    }

    pub fn get_method(&self, name: &'a str) -> Option<(&'a str, &FnDecl<'a>)> {
        self.get_method_of(self.class_name?, name)
    }

    pub fn get_method_of(
        &self,
        mut class: &'a str,
        name: &'a str,
    ) -> Option<(&'a str, &FnDecl<'a>)> {
        loop {
            let decl = self.declarations.classes().get(class)?;
            let m = decl.methods().iter().find(|m| m.0 == name);
            if let Some((_, method)) = m {
                return Some((class, method));
            }

            class = decl.parent()?;
        }
    }

    pub fn is_subclass(&self, subclass: &str, superclass: &str) -> bool {
        let mut class = self.declarations.classes().get_key_value(subclass);
        while let Some((name, decl)) = class {
            if *name == superclass {
                return true;
            }

            class = decl
                .parent()
                .and_then(|parent| self.declarations.classes().get_key_value(parent));
        }

        false
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        ast::{Arg, Def, FnDef, Leaf, Type},
        error::LatteError,
        frontend::declarations::builder::DeclarationsBuilder,
    };

    fn make_decl<'a>(defs: &[Def<'a>]) -> Result<Declarations<'a>, LatteError> {
        let mut builder = DeclarationsBuilder::default();
        for def in defs {
            match def {
                Def::Fn(fun) => builder.add_function(fun)?,
                Def::Class(class) => builder.add_class(class)?,
            }
        }
        builder.build()
    }

    #[test]
    fn no_main() {
        make_decl(&[]).expect_err("no main function should fail");
    }

    #[test]
    fn invalid_main() {
        make_decl(&[Def::Fn(FnDef {
            ret: None,
            ident: Leaf::new("main", 0),
            args: vec![],
            stmts: vec![],
        })])
        .expect_err("main returning no value should fail");

        make_decl(&[Def::Fn(FnDef {
            ret: Some(Leaf::new(Type::BOOL, 0)),
            ident: Leaf::new("main", 0),
            args: vec![],
            stmts: vec![],
        })])
        .expect_err("main returning a boolean should fail");

        make_decl(&[Def::Fn(FnDef {
            ret: Some(Leaf::new(Type::INT, 0)),
            ident: Leaf::new("main", 0),
            args: vec![Arg {
                arg_type: Leaf::new(Type::INT, 0),
                name: Leaf::new("arg", 0),
            }],
            stmts: vec![],
        })])
        .expect_err("main with arguments should fail");
    }

    #[test]
    fn only_valid_main() {
        make_decl(&[Def::Fn(FnDef {
            ret: Some(Leaf::new(Type::INT, 0)),
            ident: Leaf::new("main", 0),
            args: vec![],
            stmts: vec![],
        })])
        .expect("only valid main function should not fail");
    }

    #[test]
    fn arg_duplicate() {
        FnDecl::new(
            &FnDef {
                ret: None,
                ident: Leaf::new("fun", 0),
                args: vec![
                    Arg {
                        arg_type: Leaf::new(Type::INT, 0),
                        name: Leaf::new("arg1", 0),
                    },
                    Arg {
                        arg_type: Leaf::new(Type::BOOL, 0),
                        name: Leaf::new("arg1", 0),
                    },
                ],
                stmts: vec![],
            },
            None,
        )
        .expect_err("duplicate function argument should fail");
    }
}
