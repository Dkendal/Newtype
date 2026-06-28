extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{meta::ParseNestedMeta, parse_macro_input, Fields, Item, ItemStruct};

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
    let mut item = parse_macro_input!(input as Item);

    let mut attrs = AstNodeAttributes::default();
    let args_parser = syn::meta::parser(|meta| attrs.parse(meta));
    parse_macro_input!(args with args_parser);

    match &mut item {
        Item::Struct(item) => ast_node_struct(attrs, item),
        Item::Enum(item) => ast_node_enum(attrs, item),
        _ => panic!("Expected struct or enum"),
    }
}

fn ast_node_enum(attrs: AstNodeAttributes, item: &mut syn::ItemEnum) -> TokenStream {
    if attrs.transparent {
        panic!("Transparent enums are not supported");
    }

    // Iterate through each variant, for each variant:
    // If the variant has unnamed fields, and that field is a Span, then add
    // the #[serde(skip)] attribute to the field.
    item.attrs.push(syn::parse_quote! {
        #[derive(derivative::Derivative, serde::Serialize, Clone, Eq)]
    });

    item.attrs.push(syn::parse_quote! {
        #[derivative(PartialEq)]
    });

    item.attrs.push(syn::parse_quote! {
        #[derivative(Debug)]
    });

    item.attrs.push(syn::parse_quote! {
        #[serde(rename_all = "kebab-case")]
    });


    item.variants.iter_mut().for_each(|variant| {
        // check if the variant has the container attribute `ast_node(span)`
        // if it does, then add the span field to the variant
        // variant.attrs.iter().
        let (left, right): (Vec<_>, Vec<_>) = variant
            .attrs
            .iter()
            .partition(|attr| attr.path().is_ident("ast_node"));

        for attr in left {
            attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("span") {
                    match variant.fields {
                        Fields::Unnamed(ref mut fields) => {
                            fields.unnamed.push(syn::parse_quote! {
                                #[derivative(PartialEq = "ignore")]
                                #[serde(skip)]
                                crate::ast::Span
                            });
                        }
                        _ => panic!("Expected unnamed fields"),
                    }
                } else {
                    panic!("Unknown attribute");
                }

                Ok(())
            })
            .unwrap();
        }

        variant.attrs = right.into_iter().cloned().collect();
    });

    // Return the generated tokens
    TokenStream::from(quote! {
        #item
    })
}

fn ast_node_struct(attrs: AstNodeAttributes, item: &mut ItemStruct) -> TokenStream {
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
            pub span: crate::ast::Span
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
                fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
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

// ---------------------------------------------------------------------------
// corpus-style test generation (typescript_tests / equivalent_tests)
// ---------------------------------------------------------------------------

use proc_macro2::TokenStream as TokenStream2;
use quote::format_ident;
use std::collections::HashMap;
use std::path::{Path as StdPath, PathBuf};
use syn::parse::{Parse, ParseStream};
use syn::{LitBool, LitInt, LitStr, Path as SynPath, Token};

/// Parsed arguments for the corpus test attribute macros.
struct TsTestsArgs {
    /// Path to the `Rule` enum, e.g. `newtype::parser::Rule`.
    rule_path: SynPath,
    /// Variant of `Rule` to start parsing from, e.g. `"program"` or `"expr"`.
    rule_name: String,
    /// Directory (relative to the consuming crate's `CARGO_MANIFEST_DIR`)
    /// containing the fixtures.
    dir: String,
    /// File extension of fixtures (without the dot). Defaults to `txt`.
    ext: String,
    /// Whether to descend into subdirectories. Defaults to `false`.
    recursive: bool,
}

impl Parse for TsTestsArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        // Positional: <Rule path> , "<rule name>"
        let rule_path: SynPath = input.parse()?;
        input.parse::<Token![,]>()?;
        let rule_name: LitStr = input.parse()?;

        let mut dir: Option<String> = None;
        let mut ext = String::from("txt");
        let mut recursive = false;

        // Remaining: comma-separated `key = value` pairs.
        while input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
            if input.is_empty() {
                break; // tolerate a trailing comma
            }
            let key: syn::Ident = input.parse()?;
            input.parse::<Token![=]>()?;
            match key.to_string().as_str() {
                "dir" => dir = Some(input.parse::<LitStr>()?.value()),
                "ext" => ext = input.parse::<LitStr>()?.value(),
                "recursive" => recursive = input.parse::<LitBool>()?.value(),
                // `width` is accepted for forward-compatibility but currently
                // fixed in the runtime; parse and ignore so call sites don't break.
                "width" => {
                    let _ = input.parse::<LitInt>()?;
                }
                other => {
                    return Err(syn::Error::new(
                        key.span(),
                        format!("unknown corpus-test argument `{other}`"),
                    ))
                }
            }
        }

        let dir = dir.ok_or_else(|| {
            syn::Error::new(rule_name.span(), "a corpus-test macro requires a `dir = \"...\"` argument")
        })?;

        Ok(TsTestsArgs {
            rule_path,
            rule_name: rule_name.value(),
            dir,
            ext,
            recursive,
        })
    }
}

/// Collects fixture files under `root`, returning `(test_ident_name, absolute_path)`
/// pairs. `test_ident_name` is the path relative to `root` (without extension),
/// sanitized into a valid Rust identifier fragment.
fn collect_fixtures(root: &StdPath, ext: &str, recursive: bool, out: &mut Vec<(String, PathBuf)>) {
    let entries = match std::fs::read_dir(root) {
        Ok(entries) => entries,
        Err(_) => return,
    };

    let mut entries: Vec<_> = entries.filter_map(|e| e.ok()).map(|e| e.path()).collect();
    // Deterministic order so generated test names are stable across runs.
    entries.sort();

    for path in entries {
        if path.is_dir() {
            if recursive {
                collect_fixtures(&path, ext, recursive, out);
            }
        } else if path.extension().and_then(|e| e.to_str()) == Some(ext) {
            out.push((String::new(), path));
        }
    }
}

/// Turns a fixture path into a valid identifier fragment, relative to `base`.
fn ident_fragment(base: &StdPath, path: &StdPath) -> String {
    let rel = path.strip_prefix(base).unwrap_or(path);
    let rel = rel.with_extension("");
    let raw = rel.to_string_lossy();
    let mut name = String::with_capacity(raw.len());
    for ch in raw.chars() {
        if ch.is_ascii_alphanumeric() {
            name.push(ch);
        } else {
            name.push('_');
        }
    }
    // Identifiers can't start with a digit.
    if name.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(true) {
        name.insert(0, '_');
    }
    name
}

/// Shared implementation for the corpus-test attribute macros. `runner` is the
/// path to the `newtype::corpus` function each generated `#[test]` should call
/// with `(rule, path)` — e.g. `run_case` or `run_equivalence_case`.
fn corpus_tests_impl(attr: TokenStream, item: TokenStream, runner: TokenStream2) -> TokenStream {
    let args = parse_macro_input!(attr as TsTestsArgs);
    let mut module = match parse_macro_input!(item as Item) {
        Item::Mod(module) => module,
        other => {
            return syn::Error::new_spanned(
                other,
                "a corpus-test macro may only be applied to a module",
            )
            .to_compile_error()
            .into();
        }
    };

    let manifest_dir = std::env::var("CARGO_MANIFEST_DIR")
        .expect("CARGO_MANIFEST_DIR is set by cargo during compilation");
    let root = StdPath::new(&manifest_dir).join(&args.dir);

    let mut fixtures = Vec::new();
    collect_fixtures(&root, &args.ext, args.recursive, &mut fixtures);

    let rule_path = &args.rule_path;
    let rule_variant = format_ident!("{}", args.rule_name);

    let (_, content) = module.content.get_or_insert_with(Default::default);

    if fixtures.is_empty() {
        // Surface an obviously-failing test rather than silently generating
        // nothing when a `dir` is wrong or empty.
        let dir_lit = args.dir.clone();
        let ext_lit = args.ext.clone();
        let item: Item = syn::parse_quote! {
            #[test]
            fn no_fixtures_found() {
                panic!(
                    "found no `*.{}` fixtures under {:?}; check the `dir` argument",
                    #ext_lit, #dir_lit
                );
            }
        };
        content.push(item);
    }

    // Reject identifier collisions (e.g. `foo-bar.txt` and `foo_bar.txt` both
    // map to `foo_bar`) with a clear error instead of an opaque
    // duplicate-definition one.
    let mut seen: HashMap<String, PathBuf> = HashMap::new();
    for (_, path) in &fixtures {
        let frag = ident_fragment(&root, path);
        if let Some(prev) = seen.insert(frag.clone(), path.clone()) {
            let msg = format!(
                "corpus fixtures {:?} and {:?} both map to the test name `test_{}`; rename one",
                prev, path, frag
            );
            return syn::Error::new_spanned(&module, msg).to_compile_error().into();
        }

        let fn_name = format_ident!("test_{}", frag);
        let path_lit = path.to_string_lossy().to_string();
        let item: Item = syn::parse_quote! {
            #[test]
            fn #fn_name() {
                #runner(
                    #rule_path::#rule_variant,
                    ::std::path::Path::new(#path_lit),
                );
            }
        };
        content.push(item);
    }

    quote!(#module).into()
}

/// Generates one `#[test]` function per fixture file in `dir` for a corpus of
/// "newtype source -> expected TypeScript" tests. Mirrors the ergonomics of
/// `pest_test_gen::pest_tests`, but asserts on rendered TypeScript output
/// (via [`newtype::corpus::run_case`](../newtype/corpus/fn.run_case.html))
/// rather than on a parse tree.
///
/// # Arguments
///
/// * **rule_path** (positional, required): path to the `Rule` enum, e.g.
///   `newtype::parser::Rule`.
/// * **rule_name** (positional, required): the `Rule` variant to start parsing
///   from, e.g. `"program"`, `"expr"`, or `"if_expr"`.
/// * `dir` (required): directory of fixtures, relative to the consuming crate's
///   manifest directory.
/// * `ext` (optional): fixture file extension, default `"txt"`.
/// * `recursive` (optional): recurse into subdirectories, default `false`.
///
/// # Example
///
/// ```ignore
/// #[typescript_tests(newtype::parser::Rule, "program", dir = "tests/corpus/typescript/type_alias")]
/// #[cfg(test)]
/// mod type_alias {}
/// ```
#[proc_macro_attribute]
pub fn typescript_tests(attr: TokenStream, item: TokenStream) -> TokenStream {
    corpus_tests_impl(attr, item, quote!(newtype::corpus::run_case))
}

/// Generates one `#[test]` function per fixture file in `dir` for a corpus of
/// newtype *equivalence* tests: the two snippets in each fixture must simplify
/// to the same AST. The corpus form of the inline `assert_expr_eq!` macro,
/// backed by
/// [`newtype::corpus::run_equivalence_case`](../newtype/corpus/fn.run_equivalence_case.html).
///
/// Arguments are identical to [`macro@typescript_tests`].
///
/// # Example
///
/// ```ignore
/// #[equivalent_tests(newtype::parser::Rule, "expr", dir = "tests/corpus/newtype/expr")]
/// #[cfg(test)]
/// mod expr {}
/// ```
#[proc_macro_attribute]
pub fn equivalent_tests(attr: TokenStream, item: TokenStream) -> TokenStream {
    corpus_tests_impl(attr, item, quote!(newtype::corpus::run_equivalence_case))
}
