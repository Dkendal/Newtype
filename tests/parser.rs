use newtype::ast::Ast;
use newtype::parser::{self, NewtypeParser, Rule};
use newtype::typescript::Pretty;
use pest::{consumes_to, fails_with, parses_to, Parser};
use Rule::*;

#[macro_use]
mod common;

#[test]
fn parse_expr_sexp_apply() {
    let pairs = NewtypeParser::parse(Rule::expr, "Equals(T, any)").unwrap();
    let actual = parser::parse_expr(pairs);
    insta::assert_snapshot!(actual.to_sexp().unwrap());
}

#[test]
fn parse_expr_sexp_apply_with_path() {
    let pairs = NewtypeParser::parse(Rule::expr, "A::Equals(T, any)").unwrap();
    let actual = parser::parse_expr(pairs);
    insta::assert_snapshot!(actual.to_sexp().unwrap());
}

#[rstest::rstest]
// literals
#[case("1", "1")]
#[case("true", "true")]
#[case("false", "false")]
#[case("{}", "{}")]
// function type
#[case("() => void", "() => void")]
#[case("(any) => void", "(arg0: any) => void")]
#[case("(x: any) => void", "(x: any) => void")]
#[case("(x: any, y: any, z: any) => void", "(x: any, y: any, z: any) => void")]
#[case("(...args: any[]) => void", "(...args: any[]) => void")]
#[case("(...any[]) => void", "(...rest: any[]) => void")]
fn test_parse_expr_typescript_repr(#[case] input: &str, #[case] expected: &str) {
    let result = NewtypeParser::parse(Rule::expr, input);

    match result {
        Ok(pairs) => {
            let actual = parser::parse_expr(pairs).simplify().render_pretty_ts(80);
            pretty_assertions::assert_eq!(actual, expected);
        }
        Err(err) => {
            panic!("{}", err);
        }
    }
}

#[test]
fn parses_to_ident() {
    parses_to! {
        parser: NewtypeParser,
        input: "x",
        rule: Rule::ident,
        tokens: [ident(0, 1)]
    };
}

#[test]
fn fails_with_else() {
    fails_with! {
        parser: NewtypeParser,
        input: "else",
        rule: Rule::ident,
        positives: [ident],
        negatives: [],
        pos: 0
    };
}

fn parse_extends(input: &str) -> Ast {
    let pairs = NewtypeParser::parse(Rule::extends_expr, input).unwrap();
    parser::parse_extends_expr(pairs)
}

#[test]
fn extends_expr_parser_extends() {
    insta::assert_snapshot!(parse_extends("A <: B").to_sexp().unwrap());
}

#[test]
fn extends_expr_parser_extends_parens() {
    insta::assert_snapshot!(parse_extends("(A <: B)").to_sexp().unwrap());
}

#[test]
fn extends_expr_parser_extends_multiple_parens() {
    insta::assert_snapshot!(parse_extends("((A <: B))").to_sexp().unwrap());
}

#[test]
fn extends_expr_parser_not_with_parens_extends() {
    insta::assert_snapshot!(parse_extends("not (A <: B)").to_sexp().unwrap());
}

#[test]
fn extends_expr_parser_and() {
    insta::assert_snapshot!(parse_extends("A <: B and C <: D").to_sexp().unwrap());
}

#[test]
fn extends_expr_parser_not_and_left() {
    insta::assert_snapshot!(parse_extends("not (A <: B) and C <: D").to_sexp().unwrap());
}

#[test]
fn extends_expr_parser_not_and_right() {
    insta::assert_snapshot!(parse_extends("A <: B and (not (C <: D))").to_sexp().unwrap());
}

#[test]
fn extends_expr_parser_not_and_both() {
    insta::assert_snapshot!(
        parse_extends("not (A <: B) and (not (C <: D))")
            .to_sexp()
            .unwrap()
    );
}

#[test]
fn import_statement_named_imports() {
    assert_typescript!(
        r#"import type { A, B, C, D as D1 } from 'a';"#,
        r#"import { A, B, C, D as D1 } from :a"#
    );
}

#[test]
fn array_access_single_quote() {
    assert_typescript!(expr, "A['field']", "A['field']");
}

#[test]
fn array_access_double_quote() {
    assert_typescript!(expr, r#"A['field']"#, r#"A["field"]"#);
}

#[test]
fn array_access_template_string() {
    assert_typescript!(expr, r#"A[`${x}`]"#, r#"A[`${x}`]"#);
}

#[test]
fn dot_access() {
    assert_typescript!(expr, r#"A['x']"#, r#"A.x"#);
}

fn chained_dot_access() {
    assert_typescript!(expr, r#"A['x']['y']['z']"#, r#"A.x.y.z"#);
}

#[test]
fn chained_array_access() {
    assert_typescript!(expr, r#"A['x']['y']['z']"#, r#"A['x']['y']['z']"#);
}

#[test]
fn array_access_indexer() {
    assert_typescript!(expr, r#"A[string]"#, r#"A[string]"#);
}

mod primitives {
    const R: Rule = Rule::expr;
    use super::*;

    #[test]
    fn number() {
        assert_typescript!("type A = number;", "type A as number");
    }

    #[test]
    fn boolean() {
        assert_typescript!("type A = boolean;", "type A as boolean");
    }

    #[test]
    fn never() {
        assert_typescript!("type A = never;", "type A as never");
    }

    #[test]
    fn any() {
        assert_typescript!("type A = any;", "type A as any");
    }

    #[test]
    fn unknown() {
        assert_typescript!("type A = unknown;", "type A as unknown");
    }
}

mod literals {
    const R: Rule = Rule::expr;
    use super::*;

    #[test]
    fn number_literal_positive_integer() {
        assert_typescript!("type A = 1;", "type A as 1");
    }

    #[test]
    fn number_literal_negative_integer() {
        assert_typescript!("type A = -1;", "type A as -1");
    }

    #[test]
    fn number_literal_negative_integer_with_space() {
        assert_typescript!("type A = - 1;", "type A as - 1");
    }

    #[test]
    fn number_literal_large_integer() {
        assert_typescript!("type A = 100;", "type A as 100");
    }

    #[test]
    fn number_literal_integer_with_underscore() {
        assert_typescript!("type A = 1_000;", "type A as 1_000");
    }

    #[test]
    fn number_literal_decimal_without_fraction() {
        assert_typescript!("type A = 100.;", "type A as 100.");
    }

    #[test]
    fn number_literal_decimal_with_fraction() {
        assert_typescript!("type A = 100.0;", "type A as 100.0");
    }

    #[test]
    fn number_literal_decimal_with_fraction_and_underscore() {
        assert_typescript!("type A = 100.000_000;", "type A as 100.000_000");
    }

    #[test]
    fn string_atom_literals() {
        assert_typescript!(expr, r#"'x'"#, ":x");
    }

    #[test]
    fn string_atom_literals_space() {
        assert_typescript!(expr, r#"['$x', '--y__']"#, "[:$x, :--y__]");
    }

    #[test]
    fn string_double_quoted_literals() {
        assert_typescript!(expr, r#"'1'"#, r#""1""#);
    }

    #[test]
    fn string_single_quoted_literals() {
        assert_typescript!(expr, "'1'", "'1'");
    }

    #[test]
    fn template_string_literals() {
        assert_typescript!(expr, "`1`", "`1`");
    }

    #[test]
    fn literal_true() {
        assert_typescript!("type A = true;", "type A as true");
    }

    #[test]
    fn literal_false() {
        assert_typescript!("type A = false;", "type A as false");
    }

    #[test]
    fn literal_null() {
        assert_typescript!("type A = null;", "type A as null");
    }

    #[test]
    fn literal_undefined() {
        assert_typescript!("type A = undefined;", "type A as undefined");
    }
}

mod bin_ops {
    const R: Rule = Rule::expr;
    use rstest::rstest;

    use super::*;

    #[test]
    fn pipe_into_identifier() {
        assert_typescript!(Rule::program, r#"type A = Y<X>;"#, r#"type A as X |> Y"#);
    }

    #[test]
    fn pipe_literal() {
        assert_typescript!(Rule::program, r#"type A = Y<1>;"#, r#"type A as 1 |> Y"#);
    }

    #[test]
    fn pipe_into_call_with_args() {
        assert_typescript!(
            Rule::program,
            r#"type A = Y<X, 1>;"#,
            r#"type A as X |> Y(1)"#
        );
    }

    #[test]
    fn pipe_chained() {
        assert_typescript!(
            Rule::program,
            r#"type A = D<C<B<A, 1>>, 2, 3, 4>;"#,
            r#"type A as A |> B(1) |> C |> D(2, 3, 4)"#
        );
    }

    fn union() {
        assert_typescript!("type A = 1 | 2;", "type A as 1 | 2");
    }

    #[test]
    fn intersection() {
        assert_typescript!("type A = 1 & 2;", "type A as 1 & 2");
    }

    #[rstest]
    #[case("A | B", "A | B")]
    #[case("A & B", "A & B")]
    #[case("A | B | C", "A | B | C")]
    #[case("A | (B & C)", "A | B & C")]
    #[case("C | (A & B)", "A & B | C")]
    #[case("C | (A & B)", "(A & B) | C")]
    #[case("A | (B & C)", "A | (B & C)")]
    #[case("(A & B) | (C & D)", "(A & B) | (C & D)")]
    #[case("A | (B & C & D)", "A | (B & C & D)")]
    #[case("A | B | (C & D)", "A | B | C & D")]
    fn infix_op(#[case] ts: &str, #[case] nt: &str) {
        let source = nt.trim();
        let expected = ts.trim();
        let pairs = parse!(newtype::parser::Rule::expr, source);
        let simplified = pairs.simplify();
        let actual = simplified.render_pretty_ts(80);
        pretty_assertions::assert_eq!(expected, actual.trim());
    }
}

mod if_expr {
    const R: Rule = if_expr;
    use super::*;

    #[test]
    fn simple_if_else() {
        assert_typescript!(
            if_expr,
            r#"
            1 extends number
                ? 1
                : 0
            "#,
            r#"
            if 1 <: number then
                1
            else
                0
            end
            "#
        );
    }

    #[test]
    fn if_without_else() {
        assert_typescript!(
            if_expr,
            r#"
            1 extends number
                ? 1
                : never
            "#,
            r#"
            if 1 <: number then
                1
            end
            "#
        );
    }

    #[test]
    fn nested_if() {
        assert_typescript!(
            if_expr,
            r#"
            a extends b
                ? c extends d
                    ? x
                    : never
                : never
            "#,
            r#"
            if a <: b then
                if c <: d then
                    x
                end
            end
            "#
        );
    }

    #[test]
    fn nested_if_else() {
        assert_typescript!(
            if_expr,
            r#"
            a extends b
                ? c extends d
                    ? x
                    : never
                : never
            "#,
            r#"
            if a <: b then
                if c <: d then
                    x
                end
            end
            "#
        );

        assert_typescript!(
            if_expr,
            r#"
            a extends b
                ? c extends d
                    ? x
                    : y
                : never
            "#,
            r#"
            if a <: b then
                if c <: d then
                    x
                else
                    y
                end
            end
            "#
        );

        assert_typescript!(
            if_expr,
            r#"
            a extends b
                ? c extends d
                    ? x
                    : never
                : z
            "#,
            r#"
            if a <: b then
                if c <: d then
                    x
                end
            else
                z
            end
            "#
        );

        assert_typescript!(
            if_expr,
            r#"
            a extends b
                ? c extends d
                    ? x
                    : y
                : z
            "#,
            r#"
            if a <: b then
                if c <: d then
                    x
                else
                    y
                end
            else
                z
            end
            "#
        );
    }

    #[test]
    fn not_extends() {
        assert_typescript!(
            if_expr,
            r#"
            a extends b
                ? never
                : c
            "#,
            r#"
            if not (a <: b) then
                c
            end
            "#
        );
    }

    #[test]
    fn not_extends_else() {
        assert_typescript!(
            if_expr,
            r#"
            a extends b
                ? d
                : c
            "#,
            r#"
            if not (a <: b) then
                c
            else
                d
            end
            "#
        );
    }

    #[test]
    fn not_extends_nested() {
        assert_typescript!(
            if_expr,
            r#"
            a extends b
                ? c extends d
                    ? never
                    : x
                : never
            "#,
            r#"
            if a <: b then
                if not (c <: d) then
                    x
                end
            end
            "#
        );

        assert_typescript!(
            if_expr,
            r#"
            a extends b
                ? c extends d
                    ? y
                    : x
                : never
            "#,
            r#"
            if a <: b then
                if not (c <: d) then
                    x
                else
                    y
                end
            end
            "#
        );

        assert_typescript!(
            if_expr,
            r#"
            a extends b
                ? never
                : c extends d
                    ? x
                    : never
            "#,
            r#"
            if not (a <: b) then
                if c <: d then
                    x
                end
            end
            "#
        );

        assert_typescript!(
            if_expr,
            r#"
            a extends b
                ? never
                : c extends d
                    ? never
                    : x
            "#,
            r#"
            if not (a <: b) then
                if not (c <: d) then
                    x
                end
            end
            "#
        );
    }

    #[test]
    fn and_not() {
        assert_typescript!(
            if_expr,
            r#"
            a extends b
                ? never
                : c extends d
                    ? x
                    : never
            "#,
            r#"
            if not (a <: b) and c <: d then
                x
            end
            "#
        );

        assert_typescript!(
            if_expr,
            r#"
            a extends b
                ? c extends d
                    ? never
                    : x
                : never
            "#,
            r#"
            if a <: b and (not (c <: d)) then
                x
            end
            "#
        );
    }

    #[test]
    fn and() {
        assert_typescript!(
            "type A = a extends b ? c extends d ? e : f : f;",
            "type A as if a <: b and c <: d then e else f end"
        );
    }

    #[test]
    fn joined_conditions() {
        assert_typescript!(
            "type A = 1 extends number ? 1 : 0;",
            r#"type A as if 1 <: number then 1 else 0 end"#
        );
    }

    #[test]
    fn keyof() {
        assert_typescript!(
            if_expr,
            r#"
            x extends keyof y
                ? 1
                : never
            "#,
            "if x <: keyof(y) then 1 end"
        );
    }
}

#[test]
fn for_in() {
    assert_typescript!(
        map_expr,
        r#"
        { [k in t]: 1 }
        "#,
        r#"
        map k in t do 1 end
        "#
    );
}

mod object_literal {
    const R: Rule = object_literal;
    use super::*;

    #[test]
    fn index() {
        assert_typescript!(R, "{[k in K]: value}", "{ [k in K]: value }");
    }

    #[test]
    fn readonly_modifier() {
        assert_typescript!(R, "{readonly x: 1}", "{ readonly x: 1 }");
    }

    #[test]
    fn optional_modifier() {
        assert_typescript!(R, "{x?: 1}", "{ ?x: 1 }");
    }

    #[test]
    fn empty() {
        assert_typescript!(R, "{}", "{}");
    }

    #[test]
    fn one_key() {
        assert_typescript!(R, "{x: 1}", "{x: 1}");
    }

    #[test]
    fn many_keys() {
        assert_typescript!(R, "{x: 1, y: 2, z: 3}", "{x: 1, y: 2, z: 3}");
    }

    #[test]
    fn computed_property() {
        assert_typescript!(R, "{[K]: T}", "{[K]: T}");
    }
}

mod tuple {
    const R: Rule = Rule::tuple;
    use super::*;

    #[test]
    fn empty() {
        assert_typescript!(R, "[]", "[]");
    }

    #[test]
    fn single_element() {
        assert_typescript!(R, "[1]", "[1]");
    }

    #[test]
    fn multiple_elements() {
        assert_typescript!(R, "[1, 2, 3]", "[1, 2, 3]");
    }

    #[test]
    fn single_string_element() {
        assert_typescript!(R, r#"['sup']"#, r#"[:sup]"#);
    }
}

mod array {
    const R: Rule = Rule::expr;
    use super::*;

    #[test]
    fn two_d() {
        assert_typescript!(R, "number[]", "number[]");
    }
    #[test]
    fn four_d() {
        assert_typescript!(R, "number[][][]", "number[][][]");
    }

    #[test]
    fn of_numbers_with_parentheses() {
        assert_typescript!(R, "number[]", "(number)[]");
    }

    #[test]
    fn of_union_types() {
        assert_typescript!(R, "(number | string)[]", "(number | string)[]");
    }
}

mod type_alias {
    const R: Rule = Rule::type_alias;
    use super::*;

    #[test]
    fn generics_one_argument() {
        assert_typescript!(R, "type A<x> = x", "type A(x) as x");
    }

    #[test]
    fn generics_many_arguments() {
        assert_typescript!(R, "type A<x, y, z> = 1", "type A(x, y, z) as 1");
    }

    #[test]
    fn generic_with_guard() {
        assert_typescript!(R, "type A<x, y, z> = 1", "type A(x, y, z) as 1");
    }

    #[test]
    fn where_clause() {
        assert_typescript!(
            R,
            r#"type A<x extends number> = x"#,
            r#"type A(x) where x <: number as x"#
        );
    }

    #[test]
    fn defaults_clause() {
        assert_typescript!(
            R,
            r#"type A<x = number> = x"#,
            r#"type A(x) defaults x = number as x"#
        );
    }

    #[test]
    fn where_and_defaults_clause() {
        assert_typescript!(
            R,
            r#"type A<x extends number = number> = x"#,
            r#"type A(x) defaults x = number where x <: number as x"#
        );
    }

    #[test]
    fn exported() {
        assert_typescript!(R, "export type A = 1", "export type A as 1");
    }
}

mod interface {
    const R: Rule = Rule::interface;
    use super::*;

    #[test]
    fn interface() {
        assert_typescript!(
            R,
            r#"
            interface Foo {}
            "#,
            r#"
            interface Foo {}
            "#
        );
    }

    #[test]
    fn params() {
        assert_typescript!(
            R,
            r#"
            interface Foo<A> {}
            "#,
            r#"
            interface Foo(A) {}
            "#
        );
    }

    #[test]
    fn params_defaults() {
        assert_typescript!(
            R,
            r#"
            interface Foo<A = any> {}
            "#,
            r#"
            interface Foo(A)
            defaults A = any
            {}
            "#
        );
    }

    #[test]
    fn readonly_property() {
        assert_typescript!(
            R,
            r#"
            interface Foo {
                readonly x: number;
            }
            "#,
            r#"
            interface Foo {
                readonly x: number,
            }
            "#
        );
    }

    #[test]
    fn optional_property() {
        assert_typescript!(
            R,
            r#"
            interface Foo {
                x?: number;
            }
            "#,
            r#"
            interface Foo {
                ?x: number,
            }
            "#
        );
    }

    #[test]
    fn index_property() {
        assert_typescript!(
            R,
            r#"
            interface Foo {
                [K in T]: number;
            }
            "#,
            r#"
            interface Foo {
                [K in T]: number,
            }
            "#
        );
    }

    #[test]
    fn readonly_index_property() {
        assert_typescript!(
            R,
            r#"
            interface Foo {
                readonly [K in T]: number;
            }
            "#,
            r#"
            interface Foo {
                readonly [K in T]: number,
            }
            "#
        );
    }

    #[test]
    fn optional_index_property() {
        assert_typescript!(
            R,
            r#"
            interface Foo {
                [K in T]?: number;
            }
            "#,
            r#"
            interface Foo {
                ?[K in T]: number,
            }
            "#
        );
    }

    #[test]
    fn readonly_optional_index_property() {
        assert_typescript!(
            R,
            r#"
            interface Foo {
                readonly [K in T]?: number;
            }
            "#,
            r#"
            interface Foo {
                readonly ?[K in T]: number,
            }
            "#
        );
    }
}

mod application {
    const R: Rule = Rule::expr;
    use super::*;

    #[test]
    fn single_type_argument() {
        assert_typescript!(R, "A<1>", "A(1)");
    }

    #[test]
    fn multiple_type_arguments() {
        assert_typescript!(R, "A<1, 2, 3>", "A(1, 2, 3)");
    }

    #[test]
    fn type_argument_with_array() {
        assert_typescript!(R, "A<1, []>", "A(1, [])");
    }

    #[test]
    fn multiple_array_type_arguments() {
        assert_typescript!(R, "A<[], [], []>", "A([], [], [])");
    }

    #[test]
    fn nested_type_argument() {
        assert_typescript!(R, "A<B<1>>", "A(B(1))");
    }

    #[test]
    fn mixed_type_arguments() {
        assert_typescript!(R, "A<B, 1>", "A(B, 1)");
    }
}

mod unittest_statement {
    const R: Rule = Rule::program;
    use super::*;

    #[ignore = "whitespace issues"]
    #[test]
    fn typescript_no_output() {
        assert_typescript!(
            R,
            "",
            r#"
            unittest "test" do
                1
            end
            "#
        );
    }
}

mod unquote {
    const R: Rule = Rule::expr;
    use super::*;

    #[test]
    fn parsing() {
        let pairs = NewtypeParser::parse(R, "unquote!(1)").unwrap();
        let actual = parser::parse_expr(pairs);
        insta::assert_snapshot!(actual.to_sexp().unwrap());
    }

    #[ignore]
    #[test]
    fn evaluates_expression() {
        assert_typescript!(
            R,
            "1",
            r#"
            unquote!(
                if 1 <: number then
                    1
                else
                    0
                end
            )
            "#
        );
    }
}

#[test]
fn keyof() {
    assert_typescript!(expr, "keyof A", "keyof(A)");
}

#[test]
fn keyof_twice() {
    assert_typescript!(expr, "keyof keyof A", "keyof(keyof(A))");
}

#[test]
fn match_expr() {
    assert_typescript!(
        r#"
        type A<x> = x extends number ? 1 : x extends string ? 2 : 3;
        "#,
        r#"
        type A(x) as match x do
            number -> 1,
            string -> 2,
            else -> 3
        end
        "#
    );
}

#[test]
fn cond_expr() {
    assert_typescript!(
        r#"
        type A<x> =
            x extends number
                ? 1
                : x extends {}
                    ? x extends {a: 1}
                        ? 2
                        : never
                    : never;
        "#,
        r#"
        type A(x) as cond do
            x <: number -> 1,
            x <: {} and x <: {a: 1} -> 2,
        end
        "#
    );
}

#[test]
fn cond_expr_with_else() {
    assert_typescript!(
        r#"
        type A<x> = x extends number ? 1 : x extends {} ? x extends {a: 1} ? 2 : 3 : 3;
        "#,
        r#"
        type A(x) as cond do
            x <: number -> 1,
            x <: {} and x <: {a: 1} -> 2,
            else -> 3
        end
        "#
    );
}

#[test]
fn let_expr() {
    assert_typescript!(
        r#"
        type A = 1;
        "#,
        r#"
        type A as let a = 1 in a
        "#
    );
}

#[test]
fn let_expr_multi_definitions() {
    assert_typescript!(
        r#"
        type A = [1, 2];
        "#,
        r#"
        type A as
            let a = 1,
                b = 2,
            in [a, b]
        "#
    );
}

#[test]
fn let_expr_no_self_reference() {
    assert_typescript!(
        r#"
        type A = [1, a];
        "#,
        r#"
        type A as
            let a = 1,
                b = a,
            in [a, b]
        "#
    );
}

#[test]
fn let_expr_nested() {
    assert_typescript!(
        r#"
        type A = [[1, 1], [1, 1]];
        "#,
        r#"
        type A as
            let a = 1 in
            let b = [a, a] in
            let c = [b, b] in
            c
        "#
    );
}

#[test]
fn let_expr_shadowed() {
    assert_typescript!(
        r#"
        type A = [1, 2, 3];
        "#,
        r#"
        type A as
            let a = 1 in
            let b = a in
            let a = 2 in
            [b, a, let a = 3 in a]
        "#
    );
}
