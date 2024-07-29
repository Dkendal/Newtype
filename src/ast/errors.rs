use core::fmt;

use super::Ast;

pub struct SyntaxSugarError<'a> {
    pub node: Ast<'a>,
}

impl<'a> SyntaxSugarError<'a> {
    pub fn new(node: Ast<'a>) -> Self {
        Self { node }
    }
}

impl<'a> std::fmt::Display for SyntaxSugarError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SyntaxSugarError: expected AST to have been desugared")
    }
}

impl<'a> fmt::Debug for SyntaxSugarError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SyntaxSugarError: expected AST to have been desugared")
    }
}

impl<'a> core::error::Error for SyntaxSugarError<'a> {}

pub struct AssertionError {
    pub message: String,
}

impl fmt::Display for AssertionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl fmt::Debug for AssertionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl core::error::Error for AssertionError {}
