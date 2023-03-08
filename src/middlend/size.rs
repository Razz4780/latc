use crate::ast::{SimpleType, Type};
use std::fmt::{self, Debug, Formatter};

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Bytes {
    B8,
    B4,
    B1,
}

impl Debug for Bytes {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let as_str = match self {
            Self::B8 => "i64",
            Self::B4 => "i32",
            Self::B1 => "i8",
        };

        f.write_str(as_str)
    }
}

impl From<Bytes> for i32 {
    fn from(value: Bytes) -> Self {
        match value {
            Bytes::B8 => 8,
            Bytes::B4 => 4,
            Bytes::B1 => 1,
        }
    }
}

pub trait GetSize {
    fn size(&self) -> Bytes;
}

impl<'a> GetSize for SimpleType<'a> {
    fn size(&self) -> Bytes {
        match self {
            Self::Bool => Bytes::B1,
            Self::Class(..) => Bytes::B8,
            Self::Int => Bytes::B4,
            Self::Str => Bytes::B8,
        }
    }
}

impl<'a> GetSize for Type<'a> {
    fn size(&self) -> Bytes {
        match self {
            Self::Arr(..) => Bytes::B8,
            Self::Simple(t) => t.size(),
        }
    }
}
