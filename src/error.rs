use crate::ast::{Position, Type};
use lalrpop_util::{lexer::Token, ParseError};
use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

#[derive(Debug)]
pub struct StaticCheckError {
    pub msg: String,
    pub offset: Option<usize>,
}

impl From<ParseError<usize, Token<'_>, (String, usize)>> for StaticCheckError {
    fn from(error: ParseError<usize, Token<'_>, (String, usize)>) -> Self {
        match error {
            ParseError::ExtraToken { token } | ParseError::UnrecognizedToken { token, .. } => {
                Self {
                    msg: format!("unexpected token {}", token.1 .1),
                    offset: token.1 .0.into(),
                }
            }
            ParseError::InvalidToken { location } => Self {
                msg: "invalid token".into(),
                offset: location.into(),
            },
            ParseError::UnrecognizedEOF { .. } => Self {
                msg: "unexpected EOF".into(),
                offset: None,
            },
            ParseError::User { error } => Self {
                msg: error.0,
                offset: error.1.into(),
            },
        }
    }
}

impl StaticCheckError {
    pub fn expected_type(expected: Option<Type<'_>>, offset: usize) -> Self {
        let msg = match expected {
            Some(expected) => format!("invalid expression type, expected type {}", expected),
            None => "invalid expression type, expected void".into(),
        };

        Self {
            msg,
            offset: offset.into(),
        }
    }

    pub fn expected_arr(offset: usize) -> Self {
        Self {
            msg: "invalid expression type, expected an array".into(),
            offset: offset.into(),
        }
    }

    pub fn undefined_type(undefined: Type<'_>, offset: usize) -> Self {
        Self {
            msg: format!("undefined type \"{}\"", undefined),
            offset: offset.into(),
        }
    }

    pub fn expected_class(offset: usize) -> Self {
        Self {
            msg: "invalid expression type, expected a class".into(),
            offset: offset.into(),
        }
    }
}

macro_rules! bail {
    ($offset:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {
        return core::result::Result::Err($crate::error::StaticCheckError {
            msg: format!($fmt $(, $arg)*),
            offset: $offset.into(),
        })
    };
}

macro_rules! error {
    ($offset:expr, $fmt:literal $(, $arg:expr)* $(,)?) => {
        $crate::error::StaticCheckError {
            msg: format!($fmt $(, $arg)*),
            offset: $offset.into(),
        }
    };
}

pub(crate) use bail;
pub(crate) use error;

#[derive(Debug)]
pub struct StaticCheckErrorDisplay {
    error: StaticCheckError,
    position: Option<Position>,
}

impl Display for StaticCheckErrorDisplay {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.position.as_ref() {
            Some(position) => {
                write!(f, "{} at {}", self.error.msg, position)
            }
            None => write!(f, "{}", self.error.msg),
        }
    }
}

impl Error for StaticCheckErrorDisplay {}

#[derive(Debug)]
pub struct ErrorContext {
    line_breaks: Vec<usize>,
}

impl ErrorContext {
    pub fn new(input: &str) -> Self {
        let line_breaks = input
            .chars()
            .enumerate()
            .filter_map(|(p, c)| (c == '\n').then_some(p))
            .collect();

        Self { line_breaks }
    }

    fn position(&self, offset: usize) -> Position {
        let line_idx = match self.line_breaks.binary_search(&offset) {
            Ok(i) => i,
            Err(i) => i,
        };

        match line_idx {
            0 => Position {
                line: 1,
                column: offset + 1,
            },
            i => Position {
                line: i + 1,
                column: offset - self.line_breaks[i - 1],
            },
        }
    }

    pub fn display(&self, error: StaticCheckError) -> StaticCheckErrorDisplay {
        let position = error.offset.map(|o| self.position(o));
        StaticCheckErrorDisplay { error, position }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn ctx_new() {
        let ctx = ErrorContext::new("asd\nasd\n\n");
        assert_eq!(ctx.line_breaks, &[3, 7, 8]);

        let ctx = ErrorContext::new("");
        assert_eq!(ctx.line_breaks, &[]);
    }

    #[test]
    fn ctx_position() {
        let ctx = ErrorContext::new("asd\nassd\n\nasd");

        assert_eq!(ctx.position(0), Position { line: 1, column: 1 });
        assert_eq!(ctx.position(3), Position { line: 1, column: 4 });
        assert_eq!(ctx.position(4), Position { line: 2, column: 1 });
        assert_eq!(ctx.position(6), Position { line: 2, column: 3 });
        assert_eq!(ctx.position(12), Position { line: 4, column: 3 });
    }
}
