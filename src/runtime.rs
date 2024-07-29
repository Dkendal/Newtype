use crate::ast::*;

pub mod builtin {
    use UnionType;

    use crate::extends_result::ExtendsResult;

    use super::*;

    pub fn dbg(tree: Ast) -> Ast {
        println!("{:#?}", tree);
        tree
    }

    pub fn assert_equal<'a>(left: Ast<'a>, right: Ast<'a>) -> Ast<'a> {
        pretty_assertions::assert_eq!(left, right);

        let span = pest::Span::new(
            left.as_span().get_input(),
            left.as_span().start(),
            right.as_span().end(),
        )
        .unwrap();

        Ast::NoOp(span)
    }

    pub fn unquote(tree: Ast) -> Ast {
        let (out, _) = tree.prewalk((), &|tree, acc| {
            let span = tree.as_span();

            match tree {
                Ast::MacroCall(_) => todo!(),

                Ast::ExtendsExpr(ExtendsExpr {
                    lhs,
                    rhs,
                    then_branch,
                    else_branch,
                    ..
                }) => match lhs.is_subtype(&rhs) {
                    ExtendsResult::True => (then_branch.into(), acc),
                    ExtendsResult::False => (else_branch.into(), acc),
                    ExtendsResult::Never => (Ast::NeverKeyword(span), acc),
                    ExtendsResult::Both => {
                        let value = Ast::UnionType(UnionType {
                            types: vec![then_branch.into(), else_branch.into()],
                            span,
                        });

                        (value, acc)
                    }
                },

                Ast::MappedType(_) => todo!(),

                ref x if x.is_typescript_feature() => (tree, acc),

                x => unimplemented!("Expected AST to have been desugared {:?}", x),
            }
        });
        out
    }
}

#[cfg(test)]
mod tests {
    use crate::test_support::*;
    use pest::Parser;

    use crate::{
        parser::{self, NewtypeParser, Rule},
        runtime,
    };

    use super::*;

    mod builtin {
        use super::*;

        mod unquote {
            use super::*;
            use pretty_assertions::assert_eq;
            use serde_lexpr::to_value;

            #[test]
            fn literal() {
                assert_eq!(
                    to_value(runtime::builtin::unquote(ast!("1"))).unwrap(),
                    sexpr!("1").unwrap()
                );
            }

            #[test]
            fn if_expr() {
                assert_eq!(
                    to_value(runtime::builtin::unquote(
                        ast!("if 1 <: number then true else false end").simplify()
                    ))
                    .unwrap(),
                    sexpr!("true").unwrap()
                );
            }
        }

        mod assert_equal {
            use super::*;
            use pretty_assertions::assert_eq;
            use serde_lexpr::to_value;

            #[test]

            fn equal_values() {
                assert_eq!(
                    to_value(runtime::builtin::assert_equal(ast!("1"), ast!("1"))).unwrap(),
                    lexpr::sexp!(#"no-op")
                );
            }

            #[test]

            fn equal_values_with_whitespace() {
                assert_eq!(
                    to_value(runtime::builtin::assert_equal(ast!(" 1 "), ast!("1"))).unwrap(),
                    lexpr::sexp!(#"no-op")
                );
            }

            #[test]
            #[should_panic(expected = "assertion failed")]
            fn diff_values() {
                runtime::builtin::assert_equal(ast!("1"), ast!("2"));
            }
        }
    }
}
