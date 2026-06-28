//! Pending tests for features that are **not yet implemented** in the parser /
//! renderer / simplifier.
//!
//! Every test here is `#[ignore]`d, so a normal `cargo test` run stays green.
//! They are executable specifications you can use to drive — and verify — a
//! future implementation:
//!
//! ```text
//! cargo test --test pending -- --ignored        # run them all
//! cargo test --test pending typeof -- --ignored  # run one group
//! ```
//!
//! Each currently fails (it panics on a `todo!()`/`unreachable!()`, or asserts
//! output the parser silently drops today). When the corresponding feature is
//! implemented, its test should go green; remove the `#[ignore]` then — or, even
//! better, promote the case into the corpus under `tests/corpus/`.
//!
//! Two assertion styles are used:
//!
//! * [`assert_renders_like`] — the expected TypeScript is well-defined (the
//!   renderer already supports it; only the parser/lowering is missing). The
//!   comparison ignores insignificant whitespace so it pins behaviour without
//!   being brittle about exact spacing.
//! * [`assert_renders_ok`] — the *semantics* are an open design decision
//!   (there is no specified output yet), so we only assert the pipeline runs to
//!   a non-empty result. Tighten this into an `assert_renders_like` once the
//!   intended output is decided.

use newtype::extends_result::ExtendsResult;
use newtype::parser::Rule;

#[macro_use]
mod common;

/// Collapse runs of whitespace so comparisons aren't brittle about spacing.
fn norm(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}

/// Assert `source` (parsed from `rule`) renders to `expected`, ignoring
/// insignificant whitespace.
fn assert_renders_like(rule: Rule, source: &str, expected: &str) {
    let actual = newtype::corpus::render(rule, source.trim());
    pretty_assertions::assert_eq!(
        norm(expected),
        norm(&actual),
        "\n  source:   {}\n  rendered: {}",
        source.trim(),
        actual.trim()
    );
}

/// Assert `source` (parsed from `rule`) renders to *something* without panicking.
/// Use for features whose exact output is still an open question.
fn assert_renders_ok(rule: Rule, source: &str) {
    let actual = newtype::corpus::render(rule, source.trim());
    assert!(
        !actual.trim().is_empty(),
        "expected a non-empty render for: {}",
        source.trim()
    );
}

/// `typeof(...)` builtin. Implemented: `BuiltinKeyword` only has `Keyof`;
/// `parse_builtin` (src/parser.rs) hits `unreachable!()` for `typeof`, and the
/// renderer would print `typeof <arg>` by analogy with `keyof`.
mod typeof_builtin {
    use super::*;

    #[ignore = "typeof builtin unimplemented: parse_builtin only maps Rule::keyof (src/parser.rs)"]
    #[test]
    fn typeof_identifier() {
        assert_renders_like(Rule::expr, "typeof(A)", "typeof A");
    }

    #[ignore = "typeof builtin unimplemented: parse_builtin only maps Rule::keyof (src/parser.rs)"]
    #[test]
    fn typeof_nested() {
        assert_renders_like(Rule::expr, "typeof(keyof(A))", "typeof keyof A");
    }
}

/// `interface Foo extends Bar { ... }`. The renderer already emits the
/// `extends` clause (src/ast/pretty.rs), but `parse_interface` hardcodes
/// `extends = None` (src/parser.rs), so the clause is parsed then dropped — the
/// interface renders today as `interface Foo {}`.
mod interface_extends {
    use super::*;

    #[ignore = "interface extends dropped: parse_interface hardcodes extends = None (src/parser.rs)"]
    #[test]
    fn empty_body() {
        assert_renders_like(Rule::interface, "interface Foo extends Bar {}", "interface Foo extends Bar {}");
    }

    #[ignore = "interface extends dropped: parse_interface hardcodes extends = None (src/parser.rs)"]
    #[test]
    fn with_members() {
        assert_renders_like(
            Rule::interface,
            "interface Foo extends Bar { x: 1 }",
            "interface Foo extends Bar { x: 1; }",
        );
    }
}

/// Namespace imports: `import * as Foo from :a`. The renderer supports
/// `ImportClause::Namespace` (`* as <alias>`), but the parser lowering panics
/// with "not yet implemented" (src/parser.rs).
mod namespace_import {
    use super::*;

    #[ignore = "namespace import lowering unimplemented: src/parser.rs panics (not yet implemented)"]
    #[test]
    fn star_as_alias() {
        assert_renders_like(
            Rule::program,
            "import * as Foo from :a",
            "import type * as Foo from 'a';",
        );
    }
}

/// Mapped-type modifiers. The `MappedType` renderer already handles
/// `readonly` / optional `?` / `as` remap (src/ast/pretty.rs), but
/// `parse_map_expr` hardcodes them to `None` (src/parser.rs), so they are
/// parsed then dropped — these render today as a plain `{ [k in t]: 1 }`.
mod map_expr_modifiers {
    use super::*;

    #[ignore = "map_expr modifiers dropped: parse_map_expr hardcodes readonly/optional/remap = None (src/parser.rs)"]
    #[test]
    fn readonly() {
        assert_renders_like(Rule::map_expr, "map readonly k in t do 1 end", "{ readonly [k in t]: 1 }");
    }

    #[ignore = "map_expr modifiers dropped: parse_map_expr hardcodes readonly/optional/remap = None (src/parser.rs)"]
    #[test]
    fn optional() {
        assert_renders_like(Rule::map_expr, "map ?k in t do 1 end", "{ [k in t]?: 1 }");
    }

    #[ignore = "map_expr modifiers dropped: parse_map_expr hardcodes readonly/optional/remap = None (src/parser.rs)"]
    #[test]
    fn remap() {
        assert_renders_like(Rule::map_expr, "map k in t as r do 1 end", "{ [k in t as r]: 1 }");
    }
}

/// Equality operators in extends conditions: `=`, `!=`, `==`, `!==`.
/// `expand_to_extends` (src/ast/if_expr.rs) has `todo!()` for all four, so any
/// `if`/`cond` using them panics during `simplify()`. The exact lowering is an
/// open design decision, so these only assert the pipeline runs — replace with
/// `assert_renders_like` once the intended TypeScript is settled.
mod equality_operators {
    use super::*;

    #[ignore = "equality operator lowering is todo!() in src/ast/if_expr.rs; output semantics undecided"]
    #[test]
    fn equals() {
        assert_renders_ok(Rule::if_expr, "if a = b then c else d end");
    }

    #[ignore = "equality operator lowering is todo!() in src/ast/if_expr.rs; output semantics undecided"]
    #[test]
    fn not_equals() {
        assert_renders_ok(Rule::if_expr, "if a != b then c else d end");
    }

    #[ignore = "equality operator lowering is todo!() in src/ast/if_expr.rs; output semantics undecided"]
    #[test]
    fn strict_equals() {
        assert_renders_ok(Rule::if_expr, "if a == b then c else d end");
    }

    #[ignore = "equality operator lowering is todo!() in src/ast/if_expr.rs; output semantics undecided"]
    #[test]
    fn strict_not_equals() {
        assert_renders_ok(Rule::if_expr, "if a !== b then c else d end");
    }
}

/// Macro calls (`name!(...)`). `Ast::MacroCall` is `todo!()` in src/runtime.rs
/// and `unreachable!()` in the renderer, so any macro panics during
/// simplify/render. `unquote!` has a defined intent (evaluate its argument; see
/// the `#[ignore]`d `unquote::evaluates_expression` test in tests/parser.rs);
/// the others have no specified output yet.
mod macro_calls {
    use super::*;

    #[ignore = "macro expansion unimplemented: Ast::MacroCall is todo!() in src/runtime.rs"]
    #[test]
    fn unquote_evaluates_argument() {
        // Intended: unquote! evaluates its argument at compile time.
        assert_renders_like(Rule::expr, "unquote!(1)", "1");
    }

    #[ignore = "macro expansion unimplemented: Ast::MacroCall is todo!() in src/runtime.rs; output undecided"]
    #[test]
    fn generic_macro_call_runs() {
        assert_renders_ok(Rule::expr, "dbg!(A)");
    }
}

/// The `unittest "name" do ... end` statement. The renderer maps
/// `Ast::UnitTest` to `D::nil()` (src/ast/pretty.rs), so a unittest is meant to
/// contribute *no* TypeScript output — it is a compile-time-only construct. But
/// today a program containing a unittest emits a stray `;` line where the
/// unittest used to be (alongside the otherwise-correct rendering of the real
/// statements). The intended behaviour is that the unittest disappears entirely
/// while the surrounding type alias still renders cleanly.
mod unittest_statement {
    use super::*;

    #[ignore = "unittest emits a stray ';' instead of nothing (src/ast/pretty.rs Ast::UnitTest => D::nil)"]
    #[test]
    fn produces_no_output() {
        assert_renders_like(
            Rule::program,
            "unittest \"sanity\" do A end\ntype Foo as 1",
            "type Foo = 1;",
        );
    }
}

/// `is_subtype` (src/ast/subtype.rs) has `todo!()` arms for several
/// left-hand-side AST variants: `Access`, `ApplyGeneric`, `Array`, `Builtin`,
/// `Path`, a non-empty `TypeLiteral`, `Ident`, and `Tuple`. Each of these
/// panics with "not yet implemented" the moment it is asked whether it is a
/// subtype of anything. The intended subtyping result is undecided, so each
/// test only asserts the call *returns* an `ExtendsResult` without panicking;
/// the RHS (`string`) is chosen so no earlier match arm short-circuits the
/// answer, forcing evaluation to reach the variant's `todo!()`.
mod subtype_engine {
    use super::*;

    /// Assert `a.is_subtype(b)` returns *some* `ExtendsResult` (i.e. the engine
    /// does not panic). Currently every case below panics on a `todo!()`.
    fn assert_subtype_total(a: &str, b: &str) {
        let result = ast!(a).is_subtype(&ast!(b));
        assert!(matches!(
            result,
            ExtendsResult::True | ExtendsResult::False | ExtendsResult::Both | ExtendsResult::Never
        ));
    }

    #[ignore = "is_subtype Access LHS is todo!() in src/ast/subtype.rs"]
    #[test]
    fn access_lhs() {
        assert_subtype_total("A[B]", "string");
    }

    #[ignore = "is_subtype ApplyGeneric LHS is todo!() in src/ast/subtype.rs"]
    #[test]
    fn apply_generic_lhs() {
        assert_subtype_total("A(B)", "string");
    }

    #[ignore = "is_subtype Array LHS is todo!() in src/ast/subtype.rs"]
    #[test]
    fn array_lhs() {
        assert_subtype_total("A[]", "string");
    }

    #[ignore = "is_subtype Builtin LHS is todo!() in src/ast/subtype.rs"]
    #[test]
    fn builtin_lhs() {
        assert_subtype_total("keyof(A)", "string");
    }

    #[ignore = "is_subtype Path LHS is todo!() in src/ast/subtype.rs"]
    #[test]
    fn path_lhs() {
        assert_subtype_total("A::B", "string");
    }

    #[ignore = "is_subtype non-empty TypeLiteral LHS is todo!() in src/ast/subtype.rs"]
    #[test]
    fn type_literal_lhs() {
        assert_subtype_total("{ x: 1 }", "string");
    }

    #[ignore = "is_subtype Ident LHS is todo!() in src/ast/subtype.rs"]
    #[test]
    fn ident_lhs() {
        assert_subtype_total("Foo", "string");
    }

    #[ignore = "is_subtype Tuple LHS is todo!() in src/ast/subtype.rs"]
    #[test]
    fn tuple_lhs() {
        assert_subtype_total("[A, B]", "string");
    }
}

/// Known correctness bugs in *implemented* features (as opposed to the
/// unimplemented features above). These render the wrong TypeScript today; each
/// test asserts the correct output, so it fails now and will pass once the bug
/// is fixed. Discovered while building out the corpus.
mod known_bugs {
    use super::*;

    /// A dot access followed by an indexed access panics in the renderer
    /// ("rhs of dot access should be an ident", src/ast/pretty.rs), instead of
    /// chaining: `A.b` -> `A['b']`, then `[C]` -> `A['b'][C]`.
    #[ignore = "BUG: dot-then-index access panics in src/ast/pretty.rs (rhs of dot access should be an ident)"]
    #[test]
    fn dot_then_indexed_access() {
        assert_renders_like(Rule::expr, "A.b[C]", "A['b'][C]");
    }

    /// An intersection of parenthesised unions drops the grouping: the
    /// `IntersectionType` printer never parenthesises union members, so
    /// `(A | B) & (C | D)` renders as `A | B & C | D` — which is a *different*
    /// type (`&` binds tighter). It must keep the parens.
    #[ignore = "BUG: intersection printer drops parens around union members (src/ast/pretty.rs)"]
    #[test]
    fn intersection_of_unions_keeps_parens() {
        assert_renders_like(Rule::expr, "(A | B) & (C | D)", "(A | B) & (C | D)");
    }

    /// Same root cause, nested: a union inside an intersection inside a union
    /// loses the inner union's parens. `A | (B & (C | D))` renders today as
    /// `A | (B & C | D)`.
    #[ignore = "BUG: union nested in intersection loses parens (src/ast/pretty.rs)"]
    #[test]
    fn union_in_intersection_keeps_parens() {
        assert_renders_like(Rule::expr, "A | (B & (C | D))", "A | (B & (C | D))");
    }
}
