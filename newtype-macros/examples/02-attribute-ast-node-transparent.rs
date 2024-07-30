use newtype_macros_lib::ast_node;

#[ast_node(transparent)]
pub struct Value<'a, T> {
    pub ty: T
}
