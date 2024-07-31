use newtype_macros_lib::ast_node;

#[ast_node]
pub enum Ast {
    Add(i32, i32),
    #[ast_node(span)]
    Value(i32),
    #[ast_node(span)]
    OtherValue(),
}
