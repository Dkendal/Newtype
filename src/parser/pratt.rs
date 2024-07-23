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
            .op(Op::postfix(array_modifier))
            .op(Op::postfix(namespace_access) | Op::postfix(dot_access) | Op::postfix(indexed_access))
            .op(Op::prefix(infer))
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
