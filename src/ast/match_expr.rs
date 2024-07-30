use super::*;

#[ast_node]
pub struct MatchExpr<'a> {
    pub value: Rc<Ast<'a>>,
    pub arms: Vec<Arm<'a>>,
    pub else_arm: Rc<Ast<'a>>,
}

impl<'a> MatchExpr<'a> {
    pub fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast<'a>) -> Ast<'a>,
    {
        let mut expr = self.clone();
        expr.value = f(&self.value).into();
        expr.arms = self
            .arms
            .iter()
            .map(|arm| Arm {
                span: arm.span,
                pattern: f(&arm.pattern),
                body: f(&arm.body),
            })
            .collect();
        expr.else_arm = f(&self.else_arm).into();
        expr
    }

    pub fn simplify(&self) -> Ast<'a> {
        // Convert match arms to a series of extends expressions.
        // Allows for a single wildcard pattern ("_") to be used as the default case.
        let MatchExpr {
            value,
            arms,
            else_arm,
            ..
        } = self;

        let init: Ast<'a> = (**else_arm).clone();

        arms.iter().rev().fold(init, |acc: Ast, arm: &Arm| -> Ast {
            let Arm {
                span,
                pattern,
                body,
            } = arm;

            let span = *span;

            Ast::from(ExtendsExpr {
                span,
                lhs: value.clone(),
                rhs: pattern.clone().into(),
                then_branch: body.clone().into(),
                else_branch: acc.into(),
            })
        })
    }
}

#[ast_node]
pub struct Arm<'a> {
    pub pattern: Ast<'a>,
    pub body: Ast<'a>,
}
