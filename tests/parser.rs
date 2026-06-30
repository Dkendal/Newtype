use newtype::ast::Ast;
use newtype::parser::{self, NewtypeParser, Rule};
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
    insta::assert_snapshot!(parse_extends("A <: B and (not (C <: D))")
        .to_sexp()
        .unwrap());
}

#[test]
fn extends_expr_parser_not_and_both() {
    insta::assert_snapshot!(parse_extends("not (A <: B) and (not (C <: D))")
        .to_sexp()
        .unwrap());
}

mod unittest_statement {
    use super::*;

    #[test]
    fn parses_assert_statements() {
        let pair = NewtypeParser::parse(
            Rule::program,
            r#"
            unittest "assignability" do
                assert string <: unknown
                assert not (number <: string)
            end
            "#,
        )
        .unwrap()
        .next()
        .unwrap();

        let actual = parser::parse(pair);
        insta::assert_snapshot!(actual.to_sexp().unwrap());
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
