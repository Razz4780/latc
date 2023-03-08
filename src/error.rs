use crate::ast::{Position, Type};
use lalrpop_util::{lexer::Token, ParseError};
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub struct LatteError {
    msg: String,
    offset: Option<usize>,
}

impl LatteError {
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
    ctx: &'a ErrorContext,
}

impl<'a> Display for LatteErrorDisplay<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.error.offset {
            Some(offset) => {
                let pos = self.ctx.position(offset);
                write!(f, "{} at {}", pos, self.error.msg)
            }
            None => write!(f, "{}", self.error.msg),
        }
    }
}

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

    pub fn display<'a>(&'a self, error: &'a LatteError) -> LatteErrorDisplay<'a> {
        LatteErrorDisplay { error, ctx: self }
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
