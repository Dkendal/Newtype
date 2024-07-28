use node::Node;

use crate::ast::*;

pub mod builtin {
    use UnionType;

    use crate::extends_result::ExtendsResult;

    use super::*;

    pub fn dbg(tree: Node) -> Node {
        println!("{:#?}", tree);
        tree
    }

    pub fn assert_equal<'a>(left: Node<'a>, right: Node<'a>) -> Node<'a> {
        pretty_assertions::assert_eq!(left, right);
        Node::new(
            merge_spans(left.as_span(), right.as_span()),
            Ast::NoOp(left.as_span()),
        )
    }

    pub fn unquote(tree: Node) -> Node {
        let (out, _) = tree.prewalk((), &|tree, acc| {
            let span = tree.as_span();

            match &*tree.value {
                Ast::MacroCall(_) => todo!(),

                Ast::ExtendsExpr(ExtendsExpr {
                    lhs,
                    rhs,
                    then_branch,
                    else_branch,
                    ..
                }) => match lhs.is_subtype(rhs) {
                    ExtendsResult::True => (then_branch.into(), acc),
                    ExtendsResult::False => (else_branch.into(), acc),
                    ExtendsResult::Never => {
                        let mut tree = tree.clone();
                        tree.set_value(Box::new(Ast::NeverKeyword(span)));
                        (tree, acc)
                    }
                    ExtendsResult::Both => {
                        let mut tree = tree.clone();

                        let value = Ast::UnionType(UnionType {
                            types: vec![then_branch.into(), else_branch.into()],
                            span,
                        });

                        tree.set_value(Box::new(value));
                        (tree, acc)
                    }
                },

                Ast::MappedType(_) => todo!(),

                x if x.is_typescript_feature() => (tree, acc),

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
