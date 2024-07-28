extern crate proc_macro;


#[proc_macro_derive(AstMap)]
pub fn ast_map_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    dbg!(input);
    todo!();
}

