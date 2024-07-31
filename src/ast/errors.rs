use core::fmt;

use super::Ast;

pub struct SyntaxSugarError {
    pub node: Ast,
}

impl SyntaxSugarError {
    pub fn new(node: Ast) -> Self {
        Self { node }
    }
}

impl std::fmt::Display for SyntaxSugarError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SyntaxSugarError: expected AST to have been desugared")
    }
}

impl fmt::Debug for SyntaxSugarError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SyntaxSugarError: expected AST to have been desugared")
    }
}

impl core::error::Error for SyntaxSugarError {}

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
