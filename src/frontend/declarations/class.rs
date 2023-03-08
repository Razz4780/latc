use super::function::FnDecl;
use crate::{
    ast::{ClassDef, Type},
    error::LatteError,
};
use std::collections::HashSet;

/// A sanitized class declaration.
/// # Invariants
/// * Methods are unique.
/// * Fields are unique.
#[derive(Debug)]
pub struct ClassDecl<'a> {
    parent: Option<&'a str>,
    methods: Vec<(&'a str, FnDecl<'a>)>,
    fields: Vec<(&'a str, Type<'a>)>,
}

impl<'a> ClassDecl<'a> {
    /// Returns the name of the parent class.
    pub fn parent(&self) -> Option<&'a str> {
        self.parent
    }

    /// Returns the methods defined in this class.
    pub fn methods(&self) -> &[(&'a str, FnDecl<'a>)] {
        &self.methods
    }

    /// Returns the fields defined in this class.
    pub fn fields(&self) -> &[(&'a str, Type<'a>)] {
        &self.fields
    }

    /// Tries to sanitize the given dirty definition.
    pub fn new(dirty: &ClassDef<'a>) -> Result<Self, LatteError> {
        let mut field_names = HashSet::new();
        let mut fields = Vec::with_capacity(dirty.fields.len());
        for field in &dirty.fields {
            if !field_names.insert(field.name.inner) {
                return Err(LatteError::new_at(
                    format!(
                        "field \"{}\" already defined in class \"{}\"",
                        field.name.inner, dirty.ident.inner
                    ),
                    field.name.offset,
                ));
            }
            fields.push((field.name.inner, field.field_type.inner));
        }

        let mut method_names = HashSet::new();
        let mut methods = Vec::with_capacity(dirty.methods.len());
        for method in &dirty.methods {
            if !method_names.insert(method.ident.inner) {
                return Err(LatteError::new_at(
                    format!(
                        "method \"{}\" already defined in class \"{}\"",
                        method.ident.inner, dirty.ident.inner
                    ),
                    method.ident.offset,
                ));
            }
            let decl = FnDecl::new(method, Some(dirty.ident.inner))?;
            methods.push((method.ident.inner, decl));
        }

        Ok(Self {
            parent: dirty.parent.map(|l| l.inner),
            fields,
            methods,
        })
    }
}
