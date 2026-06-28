use newtype::extends_result::ExtendsResult;

#[macro_use]
mod common;

mod is_subtype_tests {
    use super::*;
    use rstest::rstest;

    static TRUE: ExtendsResult = ExtendsResult::True;
    static FALSE: ExtendsResult = ExtendsResult::False;
    static BOTH: ExtendsResult = ExtendsResult::Both;
    static NEVER: ExtendsResult = ExtendsResult::Never;

    #[rstest]
    // never
    #[case("never", "any", NEVER)]
    #[case("never", "unknown", NEVER)]
    #[case("never", "{}", NEVER)]
    #[case("never", "[]", NEVER)]
    #[case("never", "string", NEVER)]
    #[case("never", "number", NEVER)]
    #[case("never", "object", NEVER)]
    #[case("never", "boolean", NEVER)]
    #[case("never", "Object", NEVER)]
    #[case("never", "1", NEVER)]
    #[case("never", "'string'", NEVER)]
    #[case("never", "true", NEVER)]
    #[case("never", "false", NEVER)]
    #[case("never", "null", NEVER)]
    #[case("never", "undefined", NEVER)]
    #[case("never", "never", NEVER)]
    // any
    #[case("any", "any", TRUE)]
    #[case("any", "unknown", TRUE)]
    #[case("any", "{}", BOTH)]
    #[case("any", "[]", BOTH)]
    #[case("any", "string", BOTH)]
    #[case("any", "number", BOTH)]
    #[case("any", "boolean", BOTH)]
    #[case("any", "object", BOTH)]
    #[case("any", "Object", BOTH)]
    #[case("any", "1", BOTH)]
    #[case("any", "'string'", BOTH)]
    #[case("any", "true", BOTH)]
    #[case("any", "false", BOTH)]
    #[case("any", "null", BOTH)]
    #[case("any", "undefined", BOTH)]
    #[case("any", "never", BOTH)]
    // unknown
    #[case("unknown", "any", TRUE)]
    #[case("unknown", "unknown", TRUE)]
    #[case("unknown", "{}", FALSE)]
    #[case("unknown", "[]", FALSE)]
    #[case("unknown", "string", FALSE)]
    #[case("unknown", "number", FALSE)]
    #[case("unknown", "boolean", FALSE)]
    #[case("unknown", "object", FALSE)]
    #[case("unknown", "Object", FALSE)]
    #[case("unknown", "1", FALSE)]
    #[case("unknown", "'string'", FALSE)]
    #[case("unknown", "true", FALSE)]
    #[case("unknown", "false", FALSE)]
    #[case("unknown", "null", FALSE)]
    #[case("unknown", "undefined", FALSE)]
    #[case("unknown", "never", FALSE)]
    // {}
    #[case("{}", "any", TRUE)]
    #[case("{}", "unknown", TRUE)]
    #[case("{}", "{}", TRUE)]
    #[case("{}", "{ x: string }", FALSE)]
    #[case("{}", "[]", FALSE)]
    #[case("{}", "[{}]", FALSE)]
    #[case("{}", "{}[]", FALSE)]
    #[case("{}", "string", FALSE)]
    #[case("{}", "number", FALSE)]
    #[case("{}", "boolean", FALSE)]
    #[case("{}", "bitint", FALSE)]
    #[case("{}", "symbol", FALSE)]
    #[case("{}", "object", TRUE)]
    #[case("{}", "Object", TRUE)]
    #[case("{}", "Function", FALSE)]
    #[case("{}", "String", FALSE)]
    #[case("{}", "1", FALSE)]
    #[case("{}", "'string'", FALSE)]
    #[case("{}", "`string${var}`", FALSE)]
    #[case("{}", "true", FALSE)]
    #[case("{}", "false", FALSE)]
    #[case("{}", "null", FALSE)]
    #[case("{}", "undefined", FALSE)]
    #[case("{}", "never", FALSE)]
    // string
    #[case("string", "any", TRUE)]
    #[case("string", "unknown", TRUE)]
    #[case("string", "{}", FALSE)]
    #[case("string", "[]", FALSE)]
    #[case("string", "string", TRUE)]
    #[case("string", "number", FALSE)]
    #[case("string", "boolean", FALSE)]
    #[case("string", "Object", TRUE)]
    #[case("string", "Function", FALSE)]
    #[case("string", "String", TRUE)]
    #[case("string", "1", FALSE)]
    #[case("string", "'string'", FALSE)]
    #[case("string", "true", FALSE)]
    #[case("string", "false", FALSE)]
    #[case("string", "null", FALSE)]
    #[case("string", "undefined", FALSE)]
    #[case("string", "never", FALSE)]
    // String
    #[case("String", "any", TRUE)]
    #[case("String", "unknown", TRUE)]
    #[case("String", "{}", FALSE)]
    #[case("String", "[]", FALSE)]
    #[case("String", "string", TRUE)]
    #[case("String", "number", FALSE)]
    #[case("String", "boolean", FALSE)]
    #[case("String", "Object", TRUE)]
    #[case("String", "Function", FALSE)]
    #[case("String", "String", TRUE)]
    #[case("String", "1", FALSE)]
    #[case("String", "'string'", FALSE)]
    #[case("String", "true", FALSE)]
    #[case("String", "false", FALSE)]
    #[case("String", "null", FALSE)]
    #[case("String", "undefined", FALSE)]
    #[case("String", "never", FALSE)]
    #[trace]
    fn is_subtype(#[case] a: &str, #[case] b: &str, #[case] expected: ExtendsResult) {
        assert_eq!(ast!(a).is_subtype(&ast!(b)), expected);
    }
}

mod simplify_tests {
    use newtype::parser::{self, Rule::expr};
    use pest::Parser;

    #[test]
    fn simplify_path_access() {
        let pairs = parser::NewtypeParser::parse(parser::Rule::expr, "A::B::C::D").unwrap();
        let actual = parser::parse_expr(pairs).simplify();
        insta::assert_snapshot!(actual.to_sexp().unwrap());
    }

    #[test]
    fn simplify_basic() {
        let actual = parse!(expr, "if a <: b then c else d end").simplify();
        insta::assert_snapshot!(actual.to_sexp().unwrap());
    }

    #[test]
    fn simplify_not() {
        assert_expr_eq!(
            r#"
            if a <: b then
                d
            else
                c
            end
            "#,
            r#"
            if not (a <: b) then
                c
            else
                d
            end
            "#
        )
    }

    #[test]
    fn simplify_and() {
        assert_expr_eq!(
            r#"
            if a <: b and c <: d then
                e
            else
                f
            end
            "#,
            r#"
            if a <: b then
                if c <: d then
                    e
                else
                    f
                end
            else
                f
            end
            "#
        )
    }

    #[test]
    fn simplify_or() {
        assert_expr_eq!(
            r#"
            if a <: b or c <: d
            then e
            else f
            end
            "#,
            r#"
            if a <: b then e
            else
                if c <: d then e
                else f
                end
            end
            "#
        )
    }

    #[test]
    fn simplify_match_expr() {
        assert_expr_eq!(
            r#"
            match A do
                number -> 1,
                string -> 2,
                else -> 3
            end
            "#,
            r#"
            if A <: number then 1
            else
                if A <: string then 2
                else 3
                end
            end
            "#
        )
    }
}
