use super::{class::ClassDecl, function::FnDecl, Declarations};
use crate::{
    ast::{ClassDef, FnDef, Type},
    error::LatteError,
};
use std::collections::HashMap;

impl<'a> FnDef<'a> {
    fn required_classes(&self) -> impl '_ + Iterator<Item = (&'a str, usize)> {
        self.args.iter().filter_map(|arg| {
            arg.arg_type
                .inner
                .get_class()
                .map(|class| (class, arg.arg_type.offset))
        })
    }
}

impl<'a> ClassDef<'a> {
    fn required_classes(&self) -> impl '_ + Iterator<Item = (&'a str, usize)> {
        self.parent
            .map(|l| (l.inner, l.offset))
            .into_iter()
            .chain(self.methods.iter().flat_map(FnDef::required_classes))
            .chain(self.fields.iter().filter_map(|f| {
                f.field_type
                    .inner
                    .get_class()
                    .map(|class| (class, f.field_type.offset))
            }))
    }
}

pub struct DeclarationsBuilder<'a, 'b> {
    functions: HashMap<&'a str, FnDecl<'a>>,
    classes: HashMap<&'a str, ClassDecl<'a>>,
    unprocessed_classes: HashMap<&'a str, Vec<&'b ClassDef<'a>>>,
    undefined_classes: HashMap<&'a str, usize>,
}

impl<'a, 'b> Default for DeclarationsBuilder<'a, 'b> {
    fn default() -> Self {
        Self {
            functions: FnDecl::builtins(),
            classes: Default::default(),
            unprocessed_classes: Default::default(),
            undefined_classes: Default::default(),
        }
    }
}

impl<'a, 'b> DeclarationsBuilder<'a, 'b> {
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

        let decl = FnDecl::new(dirty, None)?;

        self.functions.insert(dirty.ident.inner, decl);
        for (required, offset) in dirty.required_classes() {
            if !self.classes.contains_key(required) {
                self.undefined_classes.insert(required, offset);
            }
        }

        Ok(())
    }

    fn check_virtual_method(
        &self,
        child_method: &FnDef<'a>,
        parent: &ClassDecl<'a>,
    ) -> Result<(), LatteError> {
        let mut parent = Some(parent);

        while let Some(parent_class) = parent.take() {
            let m = parent_class
                .methods()
                .iter()
                .find(|m| m.0 == child_method.ident.inner);
            if let Some((_, parent_method)) = m {
                if child_method.ret.map(|l| l.inner) != parent_method.ret() {
                    return Err(LatteError::new_at(
                        format!(
                            "invalid return type of method \"{}\", expected {}",
                            child_method.ident.inner,
                            parent_method
                                .ret()
                                .as_ref()
                                .map(ToString::to_string)
                                .unwrap_or_else(|| "void".into())
                        ),
                        child_method
                            .ret
                            .map(|l| l.offset)
                            .unwrap_or(child_method.ident.offset),
                    ));
                }

                if child_method.args.len() != parent_method.args().len() - 1 {
                    return Err(LatteError::new_at(
                        format!(
                            "invalid argument count of method \"{}\", expected {}",
                            child_method.ident.inner,
                            parent_method.args().len() - 1
                        ),
                        child_method.ident.offset,
                    ));
                }

                for (arg, expected) in child_method.args.iter().zip(&parent_method.args()[1..]) {
                    if arg.arg_type.inner != expected.1 {
                        return Err(LatteError::new_at(
                            format!(
                                "invalid argument type {} in a virtual method signature, expected {}",
                                arg.arg_type.inner,
                                expected.1
                            ),
                            arg.arg_type.offset
                        ));
                    }
                }

                break;
            }

            parent = parent_class
                .parent()
                .and_then(|parent| self.classes.get(parent));
        }

        Ok(())
    }

    pub fn add_class(&mut self, dirty: &'b ClassDef<'a>) -> Result<(), LatteError> {
        if self.classes.contains_key(dirty.ident.inner) {
            return Err(LatteError::new_at(
                format!("class \"{}\" already defined", dirty.ident.inner),
                dirty.ident.offset,
            ));
        };

        let parent_class = match dirty.parent {
            Some(parent) if parent.inner == dirty.ident.inner => {
                return Err(LatteError::new_at(
                    format!("subclassing an undefined class \"{}\"", parent.inner),
                    parent.offset,
                ));
            }
            Some(parent) => match self.classes.get(parent.inner) {
                Some(parent) => Some(parent),
                None => {
                    self.unprocessed_classes
                        .entry(parent.inner)
                        .or_default()
                        .push(dirty);

                    return Ok(());
                }
            },
            None => None,
        };

        let decl = ClassDecl::new(dirty)?;

        if let Some(parent) = parent_class {
            for method in &dirty.methods {
                self.check_virtual_method(method, parent)?;
            }
        }

        self.classes.insert(dirty.ident.inner, decl);
        self.undefined_classes.remove(dirty.ident.inner);
        for (required, offset) in dirty.required_classes() {
            if !self.classes.contains_key(required) {
                self.undefined_classes.insert(required, offset);
            }
        }

        let children = self
            .unprocessed_classes
            .remove(dirty.ident.inner)
            .unwrap_or_default();
        for child in children {
            self.add_class(child)?;
        }

        Ok(())
    }

    pub fn build(self) -> Result<Declarations<'a>, LatteError> {
        if let Some((class, offset)) = self.undefined_classes.into_iter().next() {
            return Err(LatteError::new_at(
                format!("undefined class \"{}\"", class),
                offset,
            ));
        }

        if let Some(child) = self.unprocessed_classes.into_values().flatten().next() {
            let parent = child
                .parent
                .expect("programmer error, only subclasses should be unprocessed");
            return Err(LatteError::new_at(
                format!("subclassing an undefined class \"{}\"", parent.inner),
                parent.offset,
            ));
        }

        if !self.functions.contains_key("main") {
            return Err(LatteError::new("\"main\" function not defined".into()));
        }

        Ok(Declarations {
            functions: self.functions,
            classes: self.classes,
        })
    }
}
