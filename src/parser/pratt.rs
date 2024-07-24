use pest::pratt_parser::PrattParser;

use crate::parser::Rule;

lazy_static::lazy_static! {
    pub(crate) static ref EXPR_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            .op(Op::infix(union, Left))
            .op(Op::infix(intersection, Left))
            .op(Op::infix(pipe, Left))
            .op(Op::postfix(application))
            .op(Op::postfix(array_modifier))
            .op(Op::prefix(infer))
            .op(Op::infix(colon2, Left) | Op::infix(dot_op, Left))
            .op(Op::postfix(indexed_access))
    };

    pub(crate) static ref EXTENDS_PARSER: PrattParser<Rule> = {
        use pest::pratt_parser::{Assoc::*, Op};
        use Rule::*;

        // Precedence is defined lowest to highest
        PrattParser::new()
            .op(
                Op::infix(or, Left)
                | Op::infix(and, Left)
            )
            .op(
                Op::infix(extends, Left)
                | Op::infix(not_extends, Left)
                | Op::infix(equals, Left)
                | Op::infix(not_equals, Left)
                | Op::infix(strict_equals, Left)
                | Op::infix(strict_not_equals, Left)
            )
            .op(Op::prefix(not))
    };
}
