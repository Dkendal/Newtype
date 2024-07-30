use newtype_macros_lib::ast_node;

#[ast_node]
pub enum Ast {
    Add(i32, i32),
    Value(i32),
}
