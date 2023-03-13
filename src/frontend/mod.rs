mod builder;
mod fn_context;

use crate::{
    ast::{BinOp, Def, SimpleType, Type, UnOp},
    context::Context,
    error::StaticCheckError,
};
use builder::FunctionBuilder;
use std::{
    collections::HashMap,
    fmt::{self, Debug, Formatter},
};

/// A globally unique function id inside a Latte program.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum FunctionId<'a> {
    /// A function in the global scope.
    Global { name: &'a str },
    /// A function in the class scope (a method).
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

/// A statically checked Latte program.
pub struct CheckedProgram<'a> {
    ctx: Context<'a>,
    defs: HashMap<FunctionId<'a>, Vec<Statement<'a>>>,
}

impl<'a> CheckedProgram<'a> {
    /// Statically checks the given [`Def`]s and creates an instance of this struct.
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

    /// Returns the [`Context`] for this program.
    pub fn ctx(&self) -> &Context<'a> {
        &self.ctx
    }

    /// Returns the implementations of functions in this program.
    pub fn defs(&self) -> &HashMap<FunctionId<'a>, Vec<Statement<'a>>> {
        &self.defs
    }
}

impl<'a> From<CheckedProgram<'a>> for Context<'a> {
    fn from(value: CheckedProgram<'a>) -> Self {
        value.ctx
    }
}

#[derive(Debug)]
pub enum Assignable<'a> {
    Local {
        var_idx: usize,
    },
    Field {
        field: &'a str,
        object: Box<Expression<'a>>,
    },
    Slot {
        array: Box<Expression<'a>>,
        index: Box<Expression<'a>>,
    },
}

impl<'a> Assignable<'a> {
    pub fn local(var_idx: usize) -> Self {
        Self::Local { var_idx }
    }

    pub fn field(field: &'a str, object: Expression<'a>) -> Self {
        Self::Field {
            field,
            object: object.into(),
        }
    }

    pub fn slot(array: Expression<'a>, index: Expression<'a>) -> Self {
        Self::Slot {
            array: array.into(),
            index: index.into(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CallAddr<'a> {
    Static { name: &'a str },
    Dynamic { method: &'a str },
    NewObject { class: &'a str },
    NewArray { inner: SimpleType<'a> },
    AddStrings,
    CmpStrings,
}

/// A statically checked statement of a Latte program.
#[derive(Debug)]
pub enum Statement<'a> {
    Cond {
        cond: Expression<'a>,
        inner_if: Vec<Statement<'a>>,
        inner_else: Vec<Statement<'a>>,
    },
    Assign {
        destination: Assignable<'a>,
        expression: Expression<'a>,
    },
    Expression(Expression<'a>),
    While {
        cond: Expression<'a>,
        inner: Vec<Statement<'a>>,
    },
    Return(Expression<'a>),
}

/// A statically checked expression of a Latte program.
#[derive(Debug, Clone)]
pub enum Expression<'a> {
    Void,
    Null(Type<'a>),
    LitInt(i32),
    LitStr(&'a str),
    LitBool(bool),
    Local {
        location: usize,
        value_type: Type<'a>,
    },
    Call {
        addr: CallAddr<'a>,
        args: Vec<Expression<'a>>,
        ret: Option<Type<'a>>,
    },
    Unary {
        op: UnOp,
        exp: Box<Expression<'a>>,
    },
    Binary {
        lhs: Box<Expression<'a>>,
        op: BinOp,
        rhs: Box<Expression<'a>>,
        value_type: SimpleType<'a>,
    },
    Slot {
        array: Box<Expression<'a>>,
        index: Box<Expression<'a>>,
        value_type: SimpleType<'a>,
    },
    Field {
        field: &'a str,
        object: Box<Expression<'a>>,
        value_type: Type<'a>,
    },
    Length {
        array: Box<Expression<'a>>,
    },
}

impl<'a> Expression<'a> {
    /// Returns a "zero" value for the given [`Type`].
    /// This value is used for uninitialized variables.
    pub fn zero(of_type: Type<'a>) -> Self {
        match of_type {
            Type::Arr(..) => Self::Null(of_type),
            Type::Simple(t) => match t {
                SimpleType::Bool => Self::LitBool(false),
                SimpleType::Int => Self::LitInt(0),
                SimpleType::Str => Self::LitStr(""),
                SimpleType::Class(..) => Self::Null(of_type),
            },
        }
    }

    /// Returns the [`Type`] returned by this expression.
    /// Only [`Expression::Void`] and calls to functions returning no value returns no type.
    pub fn get_type(&self) -> Option<Type<'a>> {
        match self {
            Self::Void => None,
            Self::Null(t) => (*t).into(),
            Self::LitBool(..) => Type::BOOL.into(),
            Self::LitInt(..) => Type::INT.into(),
            Self::LitStr(..) => Type::STR.into(),
            Self::Local { value_type, .. } => (*value_type).into(),
            Self::Call { ret, .. } => *ret,
            Self::Unary { op: UnOp::Neg, .. } => Type::INT.into(),
            Self::Unary { op: UnOp::Not, .. } => Type::BOOL.into(),
            Self::Binary { value_type, .. } => Type::Simple(*value_type).into(),
            Self::Slot { value_type, .. } => Type::Simple(*value_type).into(),
            Self::Field { value_type, .. } => (*value_type).into(),
            Self::Length { .. } => Type::INT.into(),
        }
    }

    /// Returns whether this expression is known to exit the program when evaluated.
    /// This is true only for direct calls to a builtin `error` function.
    pub fn exits(&self) -> bool {
        matches!(
            self,
            Self::Call {
                addr: CallAddr::Static { name: "error" },
                ..
            }
        )
    }
}
