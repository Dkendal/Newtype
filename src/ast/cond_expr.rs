use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct CondExpr<'a> {
    #[serde(skip)]
    pub span: Span<'a>,
    pub arms: Vec<Arm<'a>>,
    /// Unlike match and if expressions, the else arm is *not* optional
    pub else_arm: Node<'a>,
}

impl<'a> CondExpr<'a> {
    pub fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Node<'a>) -> Node<'a>,
    {
        let mut expr = self.clone();
        expr.arms = self
            .arms
            .iter()
            .map(|arm| Arm {
                span: arm.span,
                condition: f(&arm.condition),
                body: f(&arm.body),
            })
            .collect();
        expr.else_arm = f(&self.else_arm);
        expr
    }

    pub(crate) fn simplify(&self) -> Node<'a> {
        // Convert a CondExpr to a series of nested ternary expressions
        let CondExpr { arms, else_arm, .. } = self;

        let init_else: Node<'a> = (else_arm).clone();

        let acc: Node<'a> = arms.iter().rev().fold(init_else, |else_arm, arm| {
            let Arm {
                condition,
                body: then,
                ..
            } = arm;

            if_expr::expand_to_extends(condition, then, &else_arm)
        });

        acc
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
pub struct Arm<'a> {
    #[serde(skip)]
    pub span: Span<'a>,
    pub condition: Node<'a>,
    pub body: Node<'a>,
}
