use node::Node;

use crate::ast::*;

pub mod builtin {
    use crate::extends_result::ExtendsResult;

    use super::*;

    pub fn unquote(tree: Node) -> Node {
        let (out, _) = tree.prewalk((), &|tree, acc| match &*tree.value {
            Ast::MacroCall(_) => todo!(),
            Ast::ExtendsExpr(ExtendsExpr {
                lhs,
                rhs,
                then_branch,
                else_branch,
            }) => match lhs.is_subtype(rhs) {
                ExtendsResult::True => (then_branch.clone(), acc),
                ExtendsResult::False => (else_branch.clone(), acc),
                ExtendsResult::Never => {
                    let mut tree = tree.clone();
                    tree.set_value(Box::new(Ast::Never));
                    (tree, acc)
                }
                ExtendsResult::Both => {
                    let mut tree = tree.clone();
                    let value = Ast::InfixOp {
                        lhs: then_branch.clone(),
                        op: Op::Union,
                        rhs: else_branch.clone(),
                    };
                    tree.set_value(Box::new(value));
                    (tree, acc)
                }
            },
            Ast::MappedType(_) => todo!(),
            x if x.is_typescript_feature() => (tree, acc),
            x => unimplemented!("Expected AST to have been desugared {:?}", x),
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
    }
}