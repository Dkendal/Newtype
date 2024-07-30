extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{meta::ParseNestedMeta, parse_macro_input, Fields, ItemStruct};

#[derive(Default, Debug)]
struct AstNodeAttributes {
    transparent: bool,
}

impl AstNodeAttributes {
    fn parse(&mut self, meta: ParseNestedMeta) -> syn::Result<()> {
        if meta.path.is_ident("transparent") {
            self.transparent = true;
        }
        Ok(())
    }
}

#[proc_macro_attribute]
pub fn ast_node(args: TokenStream, input: TokenStream) -> TokenStream {
    let mut attrs = AstNodeAttributes::default();
    let args_parser = syn::meta::parser(|meta| attrs.parse(meta));
    parse_macro_input!(args with args_parser);

    let mut item = parse_macro_input!(input as ItemStruct);

    let ident = &item.ident;
    let generics = &item.generics;

    // Get the struct fields
    if let Fields::Named(ref mut fields) = item.fields {
        let skip = if !attrs.transparent {
            quote! {
                #[serde(skip)]
            }
        } else {
            quote! {}
        };

        fields.named.push(syn::parse_quote! {
            #[derivative(PartialEq = "ignore")]
            #skip
            pub span: pest::Span<'a>
        });
    } else {
        panic!("Expected named fields");
    };

    {
        let mut derive_list = vec![quote!(derivative::Derivative), quote!(Clone), quote!(Eq)];

        if !attrs.transparent {
            derive_list.push(quote!(serde::Serialize));
        }

        item.attrs.push(syn::parse_quote! {
            #[derive(#(#derive_list),*)]
        });
    }

    item.attrs.push(syn::parse_quote! {
        #[derivative(PartialEq)]
    });

    item.attrs.push(syn::parse_quote! {
        #[derivative(Debug)]
    });

    if !attrs.transparent {
        item.attrs.push(syn::parse_quote! {
            #[serde(rename_all = "kebab-case")]
        });
    }

    let additional = if attrs.transparent {
        let mut where_clause = quote! {};

        let field = match &item.fields {
            Fields::Named(fields) => {
                let field = fields.named.iter().next().unwrap();

                // See if field is a generic so that the serde::Serialize
                // bound can be infered.
                if let syn::Type::Path(ref ty) = field.ty {
                    let ty = &ty.path.segments;

                    if ty.len() == 1 {
                        let ty = &ty[0].ident;

                        where_clause = quote! {
                            #ty: serde::Serialize
                        };
                    }
                };

                let ident = &field.ident;

                quote! { #ident }
            }
            Fields::Unnamed(_field) => {
                quote! { 0 }
            }
            Fields::Unit => panic!("Expected named or unnamed fields"),
        };

        quote! {
            #[automatically_derived]
            impl #generics serde::Serialize for #ident #generics {
                fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
                where S: serde::Serializer, #where_clause
                {
                    self.#field.serialize(serializer)
                }
            }
        }
    } else {
        quote! {}
    };

    // Return the generated tokens
    TokenStream::from(quote! {
        #item

        #additional
    })
}
