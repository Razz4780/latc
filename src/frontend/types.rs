use crate::ast::{BinOp, SimpleType, Type, UnOp};

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
