use super::{layout::Layout, size::Bytes};
use crate::{
    ast::Type,
    frontend::CheckedProgram,
};
use std::collections::{hash_map::Entry, HashMap};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Symbol<'a> {
    Literal(&'a str),
    AddStrings,
    CmpStrings,
    NewArray,
    NewObject,
    Function(&'a str),
    VTable(&'a str),
}

#[derive(Debug, Clone, Copy)]
pub struct Field<'a> {
    of_type: Type<'a>,
    offset: i32,
}

impl<'a> Field<'a> {
    pub fn of_type(&self) -> Type<'a> {
        self.of_type
    }

    pub fn offset(&self) -> i32 {
        self.offset
    }
}

#[derive(Debug)]
pub struct Class<'a> {
    fields: HashMap<&'a str, Field<'a>>,
    methods: HashMap<&'a str, Method<'a>>,
    size: i32,
}

impl<'a> Class<'a> {
    pub fn methods(&self) -> &HashMap<&'a str, Method<'a>> {
        &self.methods
    }

    pub fn fields(&self) -> &HashMap<&'a str, Field<'a>> {
        &self.fields
    }

    pub fn size(&self) -> i32 {
        self.size
    }

    pub fn vtable_offset() -> i32 {
        0
    }

    pub fn field(&self, name: &str) -> &Field<'a> {
        self.fields.get(name).unwrap()
    }

    pub fn method(&self, name: &str) -> &Method<'a> {
        self.methods.get(name).unwrap()
    }

    fn process(declarations: &Declarations<'a>) -> HashMap<&'a str, Self> {
        let mut result = HashMap::with_capacity(declarations.classes().len());

        let mut classes_by_parent: HashMap<Option<&'a str>, Vec<(&'a str, &ClassDecl<'a>)>> =
            Default::default();
        declarations.classes().iter().for_each(|(name, decl)| {
            classes_by_parent
                .entry(decl.parent())
                .or_default()
                .push((*name, decl));
        });

        let mut processed_parents: Vec<Option<&'a str>> = vec![None];
        while let Some(parent) = processed_parents.pop() {
            for (name, decl) in classes_by_parent.remove(&parent).unwrap_or_default() {
                let parent_class = parent.and_then(|p| result.get(p));
                result.insert(name, Self::new(name, decl, parent_class));
                processed_parents.push(Some(name));
            }
        }

        result
    }

    fn new(name: &'a str, decl: &ClassDecl<'a>, parent: Option<&Class<'a>>) -> Self {
        let (mut layout, mut fields) = match parent {
            Some(parent) => (Layout::new(parent.size), parent.fields.clone()),
            None => (Layout::new(Bytes::B8.into()), Default::default()),
        };

        for (name, of_type) in decl.fields().iter().copied() {
            let offset = layout.add_entry(of_type);
            fields.insert(name, Field { of_type, offset });
        }

        let size = layout.size();

        let mut methods = parent.map(|p| p.methods.clone()).unwrap_or_default();
        let mut next_vtable_idx = methods.values().count();
        for (method_name, method_decl) in decl.methods() {
            match methods.entry(*method_name) {
                Entry::Vacant(e) => {
                    e.insert(Method {
                        vtable_offset: i32::try_from(next_vtable_idx).unwrap() * 8,
                        origin_class: name,
                        as_fun: Function::new(method_decl),
                    });
                    next_vtable_idx += 1;
                }
                Entry::Occupied(mut e) => {
                    e.get_mut().origin_class = name;
                }
            }
        }

        Self {
            fields,
            methods,
            size,
        }
    }
}

#[derive(Debug)]
pub struct Context<'a> {
    classes: HashMap<&'a str, Class<'a>>,
    functions: HashMap<&'a str, Function<'a>>,
}

impl<'a> Context<'a> {
    pub const ARRAY_ELEMS_OFFSET: i32 = 0;
    pub const ARRAY_LENGTH_OFFSET: i32 = 8;

    pub fn classes(&self) -> &HashMap<&'a str, Class<'a>> {
        &self.classes
    }

    pub fn new(program: &CheckedProgram<'a>) -> Self {
        let classes = Class::process(program.declarations());

        let functions = program
            .declarations()
            .functions()
            .iter()
            .map(|(name, decl)| (*name, Function::new(decl)))
            .collect();

        Self { functions, classes }
    }

    pub fn class(&self, name: &str) -> &Class<'a> {
        self.classes.get(name).unwrap()
    }

    pub fn function(&self, name: &str) -> &Function<'a> {
        self.functions.get(name).unwrap()
    }
}
