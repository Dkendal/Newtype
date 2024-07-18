use super::node::Node;

pub struct SyntaxSugarError<'a> {
    pub node: Node<'a>,
}

impl<'a> SyntaxSugarError<'a> {
    pub fn new(node: Node<'a>) -> Self {
        Self { node }
    }
}

impl<'a> std::fmt::Display for SyntaxSugarError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SyntaxSugarError: expected AST to have been desugared")
    }
}

impl<'a> std::fmt::Debug for SyntaxSugarError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SyntaxSugarError: expected AST to have been desugared")
    }
}

impl<'a> core::error::Error for SyntaxSugarError<'a> {}
