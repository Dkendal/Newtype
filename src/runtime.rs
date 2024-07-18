use node::AstNode;

use crate::ast::*;

pub mod builtin {
    use super::*;

    pub fn unquote(tree: AstNode) -> AstNode {
        let (out, _) = tree.prewalk((), &|tree, acc| match &*tree.value {
            Ast::MacroCall(_) => todo!(),
            _ => (tree, acc),
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

                let ast = parser::parse(pair);
                assert_eq!(runtime::builtin::unquote(ast).to_sexpr(0), expected);
            }
        }
    }
}
