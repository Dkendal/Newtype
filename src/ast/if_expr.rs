use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
pub struct Expr<'a> {
    pub condition: Node<'a>,
    pub then_branch: Node<'a>,
    pub else_branch: Option<Node<'a>>,
}

impl<'a> Expr<'a> {
    pub(crate) fn simplify(&self) -> Node<'a> {
        let else_branch = self
            .else_branch
            .as_ref()
            .map_or_else(|| node::Node::from(Ast::Never), |v| v.clone());

        expand_to_extends(&self.condition, &self.then_branch, &else_branch)
    }
}

impl<'a> PrettySexpr for Expr<'a> {
    fn pretty_sexpr(&self) -> pretty::RcDoc<()> {
        let mut vec = vec![
            D::text("if"),
            self.condition.pretty_sexpr(),
            D::text("then:"),
            self.then_branch.pretty_sexpr(),
        ];

        if let Some(else_branch) = &self.else_branch {
            vec.push(D::text("else:"));
            vec.push(else_branch.pretty_sexpr());
        }

        Ast::sexpr(vec)
    }
}

/// Expands an if expression into a series of nested ternary expressions
pub(crate) fn expand_to_extends<'a>(
    condition: &Node<'a>,
    then: &Node<'a>,
    else_arm: &Node<'a>,
) -> Node<'a> {
    // Recursive operations
    let out: Option<Node<'a>> = match &*condition.value {
        // Unary operators
        Ast::ExtendsPrefixOp { op, value } => {
            match op {
                // Swap `then` and `else` branches
                PrefixOp::Not if value.value.is_compatible_with_not_prefix_op() => {
                    Some(expand_to_extends(value, else_arm, then))
                }
                PrefixOp::Infer => todo!(),
                _ => {
                    unreachable!(
                        "Expected `not` or `infer` prefix operator, found {condition:#?}"
                    )
                }
            }
        }
        Ast::ExtendsInfixOp { lhs, op, rhs } => match op {
            InfixOp::And => {
                let then = expand_to_extends(rhs, then, else_arm);
                Some(expand_to_extends(lhs, &then, else_arm))
            }
            InfixOp::Or => {
                let else_arm = expand_to_extends(rhs, then, else_arm);
                Some(expand_to_extends(lhs, then, &else_arm))
            }
            _ => None,
        },
        _ => panic!("Expected extends operator, found {condition:#?}"),
    };

    if let Some(v) = out {
        return v;
    }

    // Terminal nodes
    match &*condition.value {
        // Binary operators
        Ast::ExtendsInfixOp { lhs, op, rhs } => {
            // TODO report a syntax error here
            // need to include spans in ASTNode<'a>
            if !lhs.value.is_typescript_feature() {
                dbg!(&lhs);
                unreachable!("value must be desugared before this point");
            }

            if !rhs.value.is_typescript_feature() {
                dbg!(rhs.value.to_sexpr(80));
                unreachable!("value must be desugared before this point");
            }

            match op {
                // Equivalent to `lhs extends rhs ? then : else`
                InfixOp::Extends => {
                    // FIXME missing span
                    Ast::from(ExtendsExpr::new(
                        lhs.clone(),
                        rhs.clone(),
                        then.clone(),
                        else_arm.clone(),
                    ))
                    .into()
                }
                InfixOp::NotExtends => {
                    // FIXME missing span
                    Ast::from(ExtendsExpr::new(
                        lhs.clone(),
                        rhs.clone(),
                        else_arm.clone(),
                        then.clone(),
                    ))
                    .into()
                }
                InfixOp::Equals => todo!("equals"),
                InfixOp::NotEquals => todo!("not equals"),
                InfixOp::StrictEquals => todo!("strict equals"),
                InfixOp::StrictNotEquals => todo!("strict not equals"),
                _ => unreachable!(),
            }
        }
        _ => panic!("Expected extends operator, found {condition:#?}"),
    }
}

