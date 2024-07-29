use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct CondExpr<'a> {
    #[serde(skip)]
    pub span: Span<'a>,
    pub arms: Vec<Arm<'a>>,
    /// Unlike match and if expressions, the else arm is *not* optional
    pub else_arm: Rc<Ast<'a>>,
}

impl<'a> CondExpr<'a> {
    pub fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast<'a>) -> Ast<'a>,
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
        expr.else_arm = f(&self.else_arm).into();
        expr
    }

    pub(crate) fn simplify(&self) -> Ast<'a> {
        // Convert a CondExpr to a series of nested ternary expressions
        let CondExpr { arms, else_arm, .. } = self;

        let init_else: Ast<'a> = else_arm.into();

        let acc: Ast<'a> = arms.iter().rev().fold(init_else, |else_arm, arm| {
            let Arm {
                condition,
                body: then,
                ..
            } = arm;

            if_expr::expand_to_extends(&condition, &then, &else_arm)
        });

        acc
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
pub struct Arm<'a> {
    #[serde(skip)]
    pub span: Span<'a>,
    pub condition: Ast<'a>,
    pub body: Ast<'a>,
}
