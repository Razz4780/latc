use std::fmt::{self, Debug, Display, Formatter};

/// A position inside an input Latte file.
#[derive(Debug, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "line {}, column {}", self.line, self.column)
    }
}

/// A leaf node inside an AST.
/// Contains an actual node and a byte offset.
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Leaf<T> {
    pub inner: T,
    /// Byte offset inside an input Latte program.
    pub offset: usize,
}

impl<T> Leaf<T> {
    pub fn new(inner: T, offset: usize) -> Self {
        Self { inner, offset }
    }

    /// Maps the inner node held by this instance.
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Leaf<U> {
        Leaf {
            inner: f(self.inner),
            offset: self.offset,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LTH,
    LE,
    GTH,
    GE,
    EQ,
    NEQ,
    And,
    Or,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum UnOp {
    Not,
    Neg,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Exp<'a> {
    Null {
        offset: usize,
        cast: Leaf<Type<'a>>,
    },
    Int(Leaf<i32>),
    Var(Leaf<&'a str>),
    Bool(Leaf<bool>),
    App {
        function: Box<Exp<'a>>,
        arguments: Vec<Exp<'a>>,
    },
    Str(Leaf<&'a str>),
    Un {
        op: Leaf<UnOp>,
        inner: Box<Exp<'a>>,
    },
    Bin {
        lhs: Box<Exp<'a>>,
        op: Leaf<BinOp>,
        rhs: Box<Exp<'a>>,
    },
    New {
        inner: Leaf<SimpleType<'a>>,
        offset: usize,
    },
    NewArr {
        inner: Leaf<SimpleType<'a>>,
        offset: usize,
        size: Box<Exp<'a>>,
    },
    Index {
        inner: Box<Exp<'a>>,
        index: Box<Exp<'a>>,
    },
    Member {
        inner: Box<Exp<'a>>,
        name: Leaf<&'a str>,
    },
}

impl<'a> Exp<'a> {
    pub fn offset(&self) -> usize {
        match self {
            Self::App { function, .. } => function.offset(),
            Self::Bin { lhs, .. } => lhs.offset(),
            Self::Bool(lit) => lit.offset,
            Self::Index { inner, .. } => inner.offset(),
            Self::Int(lit) => lit.offset,
            Self::Member { inner, .. } => inner.offset(),
            Self::New { offset, .. } => *offset,
            Self::NewArr { offset, .. } => *offset,
            Self::Null { offset, .. } => *offset,
            Self::Str(lit) => lit.offset,
            Self::Un { op, .. } => op.offset,
            Self::Var(lit) => lit.offset,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum SimpleType<'a> {
    Int,
    Str,
    Bool,
    Class(&'a str),
}

impl<'a> SimpleType<'a> {
    pub fn class(self) -> Option<&'a str> {
        match self {
            Self::Class(c) => Some(c),
            _ => None,
        }
    }
}

impl<'a> Display for SimpleType<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int => f.write_str("int"),
            Self::Str => f.write_str("string"),
            Self::Bool => f.write_str("boolean"),
            Self::Class(c) => f.write_str(c),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Type<'a> {
    Simple(SimpleType<'a>),
    Arr(SimpleType<'a>),
}

impl<'a> Display for Type<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Simple(t) => write!(f, "{}", t),
            Self::Arr(t) => write!(f, "{}[]", t),
        }
    }
}

impl<'a> Type<'a> {
    pub const INT: Self = Self::Simple(SimpleType::Int);
    pub const STR: Self = Self::Simple(SimpleType::Str);
    pub const BOOL: Self = Self::Simple(SimpleType::Bool);

    pub fn class(name: &'a str) -> Self {
        Self::Simple(SimpleType::Class(name))
    }

    pub fn referenced_class(self) -> Option<&'a str> {
        match self {
            Self::Simple(t) => t.class(),
            Self::Arr(t) => t.class(),
        }
    }

    pub fn elem_type(self) -> Option<SimpleType<'a>> {
        match self {
            Self::Arr(t) => Some(t),
            Self::Simple(..) => None,
        }
    }

    pub fn class_name(self) -> Option<&'a str> {
        match self {
            Self::Simple(SimpleType::Class(c)) => Some(c),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Item<'a> {
    pub var: Leaf<&'a str>,
    pub exp: Option<Exp<'a>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Stmt<'a> {
    Block {
        offset: usize,
        inner: Vec<Stmt<'a>>,
    },
    Empty,
    Decl {
        item_type: Leaf<Type<'a>>,
        items: Vec<Item<'a>>,
    },
    Ass {
        loc: Exp<'a>,
        exp: Exp<'a>,
    },
    Incr {
        loc: Exp<'a>,
    },
    Decr {
        loc: Exp<'a>,
    },
    Ret {
        offset: usize,
        exp: Option<Exp<'a>>,
    },
    Cond {
        offset: usize,
        cond: Exp<'a>,
        inner: Box<Stmt<'a>>,
    },
    CondElse {
        offset: usize,
        cond: Exp<'a>,
        inner_if: Box<Stmt<'a>>,
        inner_else: Box<Stmt<'a>>,
    },
    While {
        offset: usize,
        cond: Exp<'a>,
        inner: Box<Stmt<'a>>,
    },
    Exp(Exp<'a>),
    For {
        offset: usize,
        elem_type: Leaf<SimpleType<'a>>,
        elem_ident: Leaf<&'a str>,
        arr: Exp<'a>,
        inner: Box<Stmt<'a>>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Arg<'a> {
    pub arg_type: Leaf<Type<'a>>,
    pub name: Leaf<&'a str>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FnDef<'a> {
    pub ret: Option<Leaf<Type<'a>>>,
    pub ident: Leaf<&'a str>,
    pub args: Vec<Arg<'a>>,
    pub stmts: Vec<Stmt<'a>>,
}

impl<'a> FnDef<'a> {
    pub fn offset(&self) -> usize {
        self.ident.offset
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Field<'a> {
    pub field_type: Leaf<Type<'a>>,
    pub name: Leaf<&'a str>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Member<'a> {
    Field(Field<'a>),
    Method(FnDef<'a>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct ClassDef<'a> {
    pub ident: Leaf<&'a str>,
    pub parent: Option<Leaf<&'a str>>,
    pub fields: Vec<Field<'a>>,
    pub methods: Vec<FnDef<'a>>,
}

impl<'a> ClassDef<'a> {
    pub fn offset(&self) -> usize {
        self.ident.offset
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Def<'a> {
    Fn(FnDef<'a>),
    Class(ClassDef<'a>),
}

impl<'a> Def<'a> {
    pub fn offset(&self) -> usize {
        match self {
            Self::Fn(fun) => fun.offset(),
            Self::Class(class) => class.offset(),
        }
    }
}
