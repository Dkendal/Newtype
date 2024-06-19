use crate::ast::*;
use crate::transform::*;

/**
 * Simplification is a desugaring process that removes newtype specific language features,
 * replacing them with typescript compatible constructs.
 */
pub trait Simplify {
    fn simplify(&self) -> Self;
}

impl Simplify for Node {
    fn simplify(&self) -> Self {
        self.transform(&|node| match node {
            // Replace all instances of `IfExpr` with `ExtendsExpr`
            Node::IfExpr(op, then, else_) => {
                let else_ = match else_ {
                    Some(v) => v.clone(),
                    None => Box::new(Node::Never),
                };

                simplify_if_expr(op, then, &else_)
            }
            Node::MatchExpr { .. } => simplify_match_expr(node),
            Node::CondExpr { .. } => simplify_cond_expr(node),
            Node::Program(_)
            | Node::TypeAlias { .. }
            | Node::BinOp { .. }
            | Node::Ident(_)
            | Node::Number(_)
            | Node::Primitive(_)
            | Node::String(_)
            | Node::TemplateString(_)
            | Node::ExtendsPrefixOp { .. }
            | Node::ExtendsBinOp { .. }
            | Node::ExtendsExpr(_, _, _, _)
            | Node::Error(_)
            | Node::ObjectLiteral(_)
            | Node::Application(_, _)
            | Node::Never
            | Node::Any
            | Node::Unknown
            | Node::Tuple(_)
            | Node::Array(_)
            | Node::Null
            | Node::Undefined
            | Node::False
            | Node::True => node.clone(),
        })
    }
}

fn simplify_cond_expr(node: &Node) -> Node {
    // Convert a CondExpr to a series of nested ternary expressions
    let Node::CondExpr { arms, else_ } = node else {
        panic!("Expected CondExpr, found {node:#?}");
    };

    let init_else: Node = (**else_).clone();

    let acc: Node = arms.iter().rev().fold(init_else, |else_, arm| {
        let CondArm {
            condition,
            body: then,
        } = arm;
        simplify_if_expr(condition, then, &else_)
    });

    acc
}

fn simplify_if_expr(condition: &Node, then: &Node, else_: &Node) -> Node {
    match condition {
        // Unary operators
        Node::ExtendsPrefixOp { op, value } => {
            match op {
                // Swap `then` and `else` branches
                PrefixOp::Not => simplify_if_expr(value, else_, then),
                _ => todo!(),
            }
        }
        // Binary operators
        Node::ExtendsBinOp { lhs, op, rhs } => match op {
            // Equivalent to `lhs extends rhs ? then : else`
            InfixOp::Extends => Node::ExtendsExpr(
                lhs.clone(),
                rhs.clone(),
                Box::new(then.clone()),
                Box::new(else_.clone()),
            ),
            InfixOp::NotExtends => Node::ExtendsExpr(
                lhs.clone(),
                rhs.clone(),
                Box::new(else_.clone()),
                Box::new(then.clone()),
            ),
            InfixOp::Equals => todo!(),
            InfixOp::NotEquals => todo!(),
            InfixOp::StrictEquals => todo!(),
            InfixOp::StrictNotEquals => todo!(),
            InfixOp::And => {
                let then = simplify_if_expr(rhs, then, else_);
                simplify_if_expr(lhs, &then, else_)
            }
            InfixOp::Or => {
                let else_ = simplify_if_expr(rhs, then, else_);
                simplify_if_expr(lhs, then, &else_)
            }
        },
        _ => panic!("Expected extends operator, found {condition:#?}"),
    }
}

// Convert match arms to a series of extends expressions.
// Allows for a single wildcard pattern ("_") to be used as the default case.
fn simplify_match_expr(node: &Node) -> Node {
    let Node::MatchExpr { value, arms, else_ } = node else {
        panic!("Expected MatchExpr, found {node:#?}");
    };

    let init_else: Node = (**else_).clone();

    let out: Node = arms.into_iter().rev().fold(init_else, |acc, arm| -> Node {
        let MatchArm { pattern, body } = arm;

        Node::ExtendsExpr(
            value.clone(),
            Box::new(pattern.clone()),
            Box::new(body.clone()),
            Box::new(acc),
        )
    });

    out
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::ast::Node::*;
    use crate::parser::Rule::expr;
    use crate::pest::Parser;
    use crate::test_support::parse;
    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn simplify_basic() {
        assert_eq!(
            parse!(expr, "if a <: b then c else d").simplify(),
            ExtendsExpr(
                Box::new(Ident("a".to_string())),
                Box::new(Ident("b".to_string())),
                Box::new(Ident("c".to_string())),
                Box::new(Ident("d".to_string())),
            )
        )
    }

    #[test]
    fn simplify_not() {
        assert_eq!(
            parse!(expr, "if not a <: b then c else d").simplify(),
            parse!(expr, "if a <: b then d else c").simplify(),
        )
    }

    #[test]
    fn simplify_and() {
        assert_eq!(
            parse!(expr, "if a <: b and c <: d then e else f").simplify(),
            parse!(expr, "if a <: b then if c <: d then e else f else f").simplify(),
        )
    }

    #[test]
    fn simplify_or() {
        assert_eq!(
            parse!(expr, "if a <: b or c <: d then e else f").simplify(),
            parse!(expr, "if a <: b then e else if c <: d then e else f else f").simplify(),
        )
    }

    #[test]
    fn simplify_match_expr() {
        assert_eq!(
            parse!(
                expr,
                r#"
                match A do
                    number => 1,
                    string => 2,
                    else => 3
                end
                "#
            )
            .simplify(),
            parse!(
                expr,
                r#"
                if A <: number then 1
                else if A <: string then 2
                  else 3
                "#
            )
        )
    }
}
