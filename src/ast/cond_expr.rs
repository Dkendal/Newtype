use super::*;

#[ast_node]
pub struct CondExpr {
    pub arms: Vec<Arm>,
    /// Unlike match and if expressions, the else arm is *not* optional
    pub else_arm: Rc<Ast>,
}

impl CondExpr {
    pub fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
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

    pub(crate) fn simplify(&self) -> Ast {
        // Convert a CondExpr to a series of nested ternary expressions
        let CondExpr { arms, else_arm, .. } = self;

        let init_else: Ast = else_arm.into();

        let acc: Ast = arms.iter().rev().fold(init_else, |else_arm, arm| {
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

#[ast_node]
pub struct Arm {
    pub condition: Ast,
    pub body: Ast,
}
