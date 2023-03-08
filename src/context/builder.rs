use super::{
    class::Class,
    function::{self, Function},
    Context,
};
use crate::{
    ast::{ClassDef, FnDef, Leaf, Type},
    error::LatteError,
};
use std::{collections::HashMap, rc::Rc};

impl<'a> FnDef<'a> {
    fn visit_referenced_types<F: FnMut(&Leaf<Type<'a>>)>(&self, f: F) {
        if let Some(r) = self.ret.as_ref() {
            f(r);
        }
        for arg in &self.args {
            f(&arg.arg_type);
        }
    }
}

impl<'a> ClassDef<'a> {
    fn visit_referenced_types<F: FnMut(&Leaf<Type<'a>>)>(&self, f: F) {
        if let Some(parent) = self.parent.as_ref() {
            f(&Leaf {
                inner: Type::class(parent.inner),
                offset: parent.offset,
            });
        }

        for field in &self.fields {
            f(&field.field_type);
        }

        for method in &self.methods {
            method.visit_referenced_types(f);
        }
    }
}

pub struct ContextBuilder<'a> {
    functions: HashMap<&'a str, Function<'a>>,
    classes: HashMap<&'a str, Rc<Class<'a>>>,
    class_references: HashMap<&'a str, usize>,
}

impl<'a> Default for ContextBuilder<'a> {
    fn default() -> Self {
        Self {
            functions: function::public_builtins()
                .into_iter()
                .map(|f| (f.name(), f))
                .collect(),
            classes: Default::default(),
            class_references: Default::default(),
        }
    }
}

impl<'a> ContextBuilder<'a> {
    pub fn add_function(&mut self, dirty: &FnDef<'a>) -> Result<(), LatteError> {
        if self.functions.contains_key(dirty.ident.inner) {
            return Err(LatteError::new_at(
                format!("function \"{}\" already defined", dirty.ident.inner),
                dirty.ident.offset,
            ));
        }

        if dirty.ident.inner == "main" {
            let returns_int = dirty.ret.as_ref().map(|l| l.inner) == Some(Type::INT);
            if !returns_int {
                return Err(LatteError::new_at(
                    "\"main\" function must return a value of type int".into(),
                    dirty.offset(),
                ));
            }

            if !dirty.args.is_empty() {
                return Err(LatteError::new_at(
                    "\"main\" function must not take any arguments".into(),
                    dirty.offset(),
                ));
            }
        }

        let decl = Function::new(dirty, None)?;

        self.functions.insert(dirty.ident.inner, decl);

        dirty.visit_referenced_types(|t| {
            if let Some(class) = t.inner.referenced_class() {
                self.class_references.entry(class).or_insert(t.offset);
            }
        });

        Ok(())
    }

    pub fn add_class(&mut self, dirty: &ClassDef<'a>) -> Result<(), LatteError> {
        if self.classes.contains_key(dirty.ident.inner) {
            return Err(LatteError::new_at(
                format!("class \"{}\" already defined", dirty.ident.inner),
                dirty.ident.offset,
            ));
        };

        let parent = match dirty.parent {
            Some(parent) if parent.inner == dirty.ident.inner => {
                return Err(LatteError::new_at(
                    format!("subclassing an undefined class \"{}\"", parent.inner),
                    parent.offset,
                ));
            }
            Some(parent) => Some(self.classes.get(parent.inner).cloned().ok_or_else(|| {
                LatteError::new_at(format!("undefined class {}", parent.inner), parent.offset)
            })?),
            None => None,
        };

        let decl = Class::new(dirty, parent)?;
        self.classes.insert(dirty.ident.inner, decl.into());

        dirty.visit_referenced_types(|t| {
            if let Some(class) = t.inner.referenced_class() {
                self.class_references.entry(class).or_insert(t.offset);
            }
        });

        Ok(())
    }

    pub fn build(self) -> Result<Context<'a>, LatteError> {
        for (class, offset) in self.class_references.into_iter() {
            if !self.classes.contains_key(class) {
                return Err(LatteError::new_at(
                    format!("undefined class \"{}\"", class),
                    offset,
                ));
            }
        }

        if !self.functions.contains_key("main") {
            return Err(LatteError::new("\"main\" function not defined".into()));
        }

        Ok(Context {
            functions: self.functions,
            classes: self.classes,
        })
    }
}
