use newtype::compose;

use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct IfExpr<'a> {
    #[serde(skip)]
    pub span: Span<'a>,
    pub condition: Rc<Ast<'a>>,
    pub then_branch: Rc<Ast<'a>>,
    pub else_branch: Option<Rc<Ast<'a>>>,
}

impl<'a> IfExpr<'a> {
    pub fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast<'a>) -> Ast<'a>,
    {
        Self {
            span: self.span,
            condition: f(&self.condition).into(),
            then_branch: f(&self.then_branch).into(),
            else_branch: self.else_branch.as_ref().map(compose!(f, Into::into)),
        }
    }

    pub fn simplify(&self) -> Ast<'a> {
        let else_branch = self
            .else_branch
            .as_ref()
            .map_or_else(|| Ast::NeverKeyword(self.span), |v| (**v).clone());

        expand_to_extends(&self.condition, &self.then_branch, &else_branch).into()
    }
}

/// Expands an if expression into a series of nested ternary expressions
pub(crate) fn expand_to_extends<'a>(
    condition: &Ast<'a>,
    then: &Ast<'a>,
    else_arm: &Ast<'a>,
) -> Ast<'a> {
    // Recursive operations
    let out: Option<Ast<'a>> = match condition {
        // Unary operators
        Ast::ExtendsPrefixOp(ExtendsPrefixOp { op, value, .. }) => {
            match op {
                // Swap `then` and `else` branches
                PrefixOp::Not if value.is_compatible_with_not_prefix_op() => {
                    Some(expand_to_extends(value, else_arm, then))
                }
                PrefixOp::Infer => todo!(),
                _ => {
                    unreachable!("Expected `not` or `infer` prefix operator, found {condition:#?}")
                }
            }
        }
        Ast::ExtendsInfixOp(ExtendsInfixOp { lhs, op, rhs, .. }) => match op {
            InfixOp::And => {
                let then = expand_to_extends(&rhs, then, else_arm);
                Some(expand_to_extends(&lhs, &then, else_arm))
            }
            InfixOp::Or => {
                let else_arm = expand_to_extends(&rhs, then, else_arm);
                Some(expand_to_extends(&lhs, then, &else_arm))
            }
            _ => None,
        },
        _ => panic!("Expected extends operator, found {condition:#?}"),
    };

    if let Some(v) = out {
        return v;
    }

    // Terminal nodes
    match condition {
        // Binary operators
        Ast::ExtendsInfixOp(ExtendsInfixOp { lhs, op, rhs, .. }) => {
            let span = Span::new(
                lhs.as_span().get_input(),
                lhs.as_span().start(),
                rhs.as_span().start(),
            )
            .unwrap();

            // TODO report a syntax error here
            // need to include spans in ASTNode<'a>
            if !lhs.is_typescript_feature() {
                dbg!(lhs);
                unreachable!("value must be desugared before this point");
            }

            if !rhs.is_typescript_feature() {
                dbg!(rhs);
                unreachable!("value must be desugared before this point");
            }

            match op {
                // Equivalent to `lhs extends rhs ? then : else`
                InfixOp::Extends => Ast::from(ExtendsExpr::new(
                    span,
                    lhs.clone(),
                    rhs.clone(),
                    Rc::new(then.clone()),
                    Rc::new(else_arm.clone()),
                )),
                InfixOp::NotExtends => Ast::from(ExtendsExpr::new(
                    span,
                    lhs.clone(),
                    rhs.clone(),
                    Rc::new(else_arm.clone()),
                    Rc::new(then.clone()),
                )),
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
