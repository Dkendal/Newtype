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
    pub(crate) fn simplify(&self) -> Node<'a> {
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

                let span = span.clone();

                // FIXME missing span
                Node {
                    span: Some(span),
                    value: Box::new(Ast::from(ExtendsExpr {
                        span,
                        lhs: value.clone(),
                        rhs: pattern.clone(),
                        then_branch: body.clone(),
                        else_branch: acc,
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
