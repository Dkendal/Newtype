use std::rc::Rc;

use newtype_macros_lib::ast_node;

#[derive(Debug, Clone, Eq, PartialEq, serde_derive::Serialize)]
pub enum Ast {
    Add(Rc<Ast>, Rc<Ast>),
    Value(i32),
}

#[ast_node]
pub struct ExtendsExpr {
    pub lhs: Rc<Ast>,
    pub rhs: Rc<Ast>,
    pub then_branch: Rc<Ast>,
    pub else_branch: Rc<Ast>,
}
