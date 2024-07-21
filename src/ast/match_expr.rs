use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
pub struct Expr<'a> {
    pub value: Node<'a>,
    pub arms: Vec<Arm<'a>>,
    pub else_arm: Node<'a>,
}

impl<'a> Expr<'a> {
    pub(crate) fn simplify(&self) -> Node<'a> {
        // Convert match arms to a series of extends expressions.
        // Allows for a single wildcard pattern ("_") to be used as the default case.
        let Expr {
            value,
            arms,
            else_arm,
        } = self;

        arms.iter()
            .rev()
            .fold(else_arm.clone(), |acc: Node, arm: &Arm| -> Node {
                let Arm { pattern, body } = arm;

                // FIXME missing span
                Node {
                    span: None,
                    value: Box::new(Ast::from(ExtendsExpr::new(
                        value.clone(),
                        pattern.clone(),
                        body.clone(),
                        acc,
                    ))),
                }
            })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
pub struct Arm<'a> {
    pub pattern: Node<'a>,
    pub body: Node<'a>,
}
