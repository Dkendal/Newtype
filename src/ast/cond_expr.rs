use super::*;

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
pub struct Expr<'a> {
    pub arms: Vec<Arm<'a>>,
    /// Unlike match and if expressions, the else arm is *not* optional
    pub else_arm: Node<'a>,
}

impl<'a> Expr<'a> {
    pub(crate) fn simplify(&self) -> Node<'a> {
        // Convert a CondExpr to a series of nested ternary expressions
        let Expr { arms, else_arm } = self;

        let init_else: Node<'a> = (else_arm).clone();

        let acc: Node<'a> = arms.iter().rev().fold(init_else, |else_arm, arm| {
            let Arm {
                condition,
                body: then,
            } = arm;

            if_expr::expand_to_extends(condition, then, &else_arm)
        });

        acc
    }
}

impl<'a> PrettySexpr for Expr<'a> {
    fn pretty_sexpr(&self) -> pretty::RcDoc<()> {
        let mut vec = vec![pretty::RcDoc::text("cond")];

        for arm in &self.arms {
            vec.push(arm.pretty_sexpr());
        }

        vec.push(self.else_arm.pretty_sexpr());

        super::Ast::sexpr(vec)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
pub struct Arm<'a> {
    pub condition: Node<'a>,
    pub body: Node<'a>,
}

impl<'a> PrettySexpr for Arm<'a> {
    fn pretty_sexpr(&self) -> pretty::RcDoc<()> {
        super::Ast::sexpr(vec![
            pretty::RcDoc::text("when"),
            self.condition.pretty_sexpr(),
        ])
    }
}

