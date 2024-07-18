use node::Node;

use crate::ast::*;

pub mod builtin {
    use super::*;

    pub fn unquote(tree: Node) -> Node {
        let (out, _) = tree.prewalk((), &|tree, acc| match &*tree.value {
            Ast::MacroCall(_) => todo!(),
            Ast::ExtendsExpr(ExtendsExpr {
                lhs,
                rhs,
                then_branch,
                else_branch,
            }) => {
                // lhs.is_extension(rhs);
                // dbg!(lhs, rhs );
                todo!();
            }
            Ast::MappedType(_) => todo!(),
            x if x.is_typescript_feature() => (tree, acc),
            x => unimplemented!("Expected AST to have been desugared {:?}", x),
        });
        out
    }
}

#[cfg(test)]
mod tests {
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

            #[test]
            fn literal() {
                let source = "1";

                let expected = "1";

                let pair = NewtypeParser::parse(Rule::expr, source)
                    .unwrap()
                    .next()
                    .unwrap();

                let ast = parser::parse(pair);
                assert_eq!(runtime::builtin::unquote(ast).to_sexpr(0), expected);
            }

            #[test]
            fn if_expr() {
                let source = "if 1 <: number then true else false end";

                let expected = "true";

                let pair = NewtypeParser::parse(Rule::expr, source)
                    .unwrap()
                    .next()
                    .unwrap();

                let ast = parser::parse(pair).simplify();
                assert_eq!(runtime::builtin::unquote(ast).to_sexpr(0), expected);
            }
        }
    }
}
