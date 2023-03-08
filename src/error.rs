use crate::ast::{Lines, Type};
use lalrpop_util::{lexer::Token, ParseError};
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub struct LatteError {
    msg: String,
    offset: Option<usize>,
}

impl LatteError {
    pub fn display<'a>(&'a self, lines: &'a Lines) -> LatteErrorDisplay<'a> {
        LatteErrorDisplay { error: self, lines }
    }

    pub fn new(msg: String) -> Self {
        Self { msg, offset: None }
    }

    pub fn new_at(msg: String, offset: usize) -> Self {
        Self {
            msg,
            offset: Some(offset),
        }
    }

    pub fn expected_type(expected: Option<Type<'_>>, offset: usize) -> Self {
        let msg = match expected {
            Some(expected) => format!("invalid expression type, expected type {}", expected),
            None => "invalid expression type, expected void".into(),
        };

        Self::new_at(msg, offset)
    }

    pub fn expected_arr(offset: usize) -> Self {
        Self::new_at("invalid expression type, expected an array".into(), offset)
    }

    pub fn undefined_type(undefined: Type<'_>, offset: usize) -> Self {
        Self::new_at(format!("undefined type \"{}\"", undefined), offset)
    }

    pub fn expected_class(offset: usize) -> Self {
        Self::new_at("invalid expression type, expected a class".into(), offset)
    }
}

impl From<ParseError<usize, Token<'_>, (String, usize)>> for LatteError {
    fn from(error: ParseError<usize, Token<'_>, (String, usize)>) -> Self {
        match error {
            ParseError::ExtraToken { token } | ParseError::UnrecognizedToken { token, .. } => {
                Self::new_at(format!("unexpected token {}", token.1 .1), token.1 .0)
            }
            ParseError::InvalidToken { location } => Self::new_at("invalid token".into(), location),
            ParseError::UnrecognizedEOF { .. } => Self::new("unexpected EOF".into()),
            ParseError::User { error } => Self::new_at(error.0, error.1),
        }
    }
}

pub struct LatteErrorDisplay<'a> {
    error: &'a LatteError,
    lines: &'a Lines,
}

impl<'a> Display for LatteErrorDisplay<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.error.offset {
            Some(offset) => {
                let pos = self.lines.position(offset);
                write!(f, "error at {}: {}", pos, self.error.msg)
            }
            None => write!(f, "error: {}", self.error.msg),
        }
    }
}
