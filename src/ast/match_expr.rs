use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct MatchExpr<'a> {
    #[serde(skip)]
    pub span: Span<'a>,
    pub value: Node<'a>,
    pub arms: Vec<Arm<'a>>,
    pub else_arm: Node<'a>,
}

impl<'a> MatchExpr<'a> {
    pub fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Node<'a>) -> Node<'a>,
    {
        let mut expr = self.clone();
        expr.value = f(&self.value);
        expr.arms = self
            .arms
            .iter()
            .map(|arm| Arm {
                span: arm.span,
                pattern: f(&arm.pattern),
                body: f(&arm.body),
            })
            .collect();
        expr.else_arm = f(&self.else_arm);
        expr
    }

    pub fn simplify(&self) -> Node<'a> {
        // Convert match arms to a series of extends expressions.
        // Allows for a single wildcard pattern ("_") to be used as the default case.
        let MatchExpr {
            value,
            arms,
            else_arm,
            ..
        } = self;

        arms.iter()
            .rev()
            .fold(else_arm.clone(), |acc: Node, arm: &Arm| -> Node {
                let Arm {
                    span,
                    pattern,
                    body,
                } = arm;

                let span = *span;

                Node {
                    span,
                    value: Box::new(Ast::from(ExtendsExpr {
                        span,
                        lhs: value.into(),
                        rhs: pattern.into(),
                        then_branch: body.into(),
                        else_branch: acc.into(),
                    })),
                }
            })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
pub struct Arm<'a> {
    #[serde(skip)]
    pub span: Span<'a>,
    pub pattern: Node<'a>,
    pub body: Node<'a>,
}
