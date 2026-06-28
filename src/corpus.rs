//! Runtime support for the *corpus* tests.
//!
//! Two corpora share this machinery, both modelled on the `pest-test` corpus
//! under `tests/pest` but asserting on rendered output rather than a parse tree:
//!
//! * **TypeScript corpus** (`tests/corpus/typescript`, [`run_case`]): assert that
//!   newtype source renders to the expected TypeScript.
//! * **Equivalence corpus** (`tests/corpus/newtype`, [`run_equivalence_case`]):
//!   assert that two newtype snippets simplify to the same AST (the corpus form
//!   of the inline `assert_expr_eq!` macro).
//!
//! The list of test functions is generated at compile time by the
//! `typescript_tests` / `equivalent_tests` attribute macros in
//! `newtype_macros_lib`; each generated function calls one of the runners below
//! at run time, so editing a fixture's *body* does not require a recompile.
//!
//! ## A note on adding/removing fixtures
//!
//! Adding or removing a fixture *file* changes the set of generated tests, which
//! only happens when the proc-macro re-runs. Cargo will not notice a new or
//! deleted fixture on its own, so the crate ships a `build.rs` that emits
//! `cargo:rerun-if-changed=tests/corpus` to force a rebuild when the corpus
//! directory changes. (Editing an existing fixture's contents needs no rebuild —
//! the runner reads the file at run time.)
//!
//! # Fixture format
//!
//! A fixture file has exactly three sections separated by a line consisting of
//! three or more `=` characters:
//!
//! ```text
//! Name of the test
//!
//! =======
//!
//! <first newtype snippet>
//!
//! =======
//!
//! <expected output (TypeScript, or the equivalent newtype snippet)>
//! ```
//!
//! Only the first two separators split the file; any further `===` lines are
//! kept verbatim as part of the third section, so output that legitimately
//! contains such a line is preserved. Each section is dedented (common leading
//! indentation stripped) and trimmed, mirroring the inline `assert_typescript!`
//! / `assert_expr_eq!` macros so fixtures and inline tests stay consistent.

use crate::ast::Ast;
use crate::parser::{self, NewtypeParser, Rule};
use crate::typescript::Pretty;
use pest::Parser;
use std::path::Path;

/// Width passed to the pretty-printer. Matches the value used by the inline
/// `assert_typescript!` tests so fixtures and inline tests stay consistent.
const RENDER_WIDTH: usize = 80;

/// A parsed corpus fixture: a human-readable name and the two snippets. For the
/// TypeScript corpus, `expected` is rendered TypeScript; for the equivalence
/// corpus, it is a second newtype snippet that must simplify to the same AST as
/// `source`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Case {
    pub name: String,
    pub source: String,
    pub expected: String,
}

/// Returns `true` for a separator line: trimmed, non-empty, and made up solely
/// of `=` characters (at least three).
fn is_separator(line: &str) -> bool {
    let line = line.trim();
    line.len() >= 3 && line.chars().all(|c| c == '=')
}

/// Strips the common leading whitespace shared by every non-blank line, then
/// trims surrounding blank lines — the run-time equivalent of the `dedent!`
/// applied by the inline test macros.
fn dedent_trim(section: &str) -> String {
    let indent = section
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| l.len() - l.trim_start().len())
        .min()
        .unwrap_or(0);

    let dedented: Vec<&str> = section
        .lines()
        .map(|l| if l.len() >= indent { &l[indent..] } else { l })
        .collect();

    dedented.join("\n").trim().to_string()
}

/// Splits a fixture file's contents into its three sections. Only the first two
/// separator lines split the file; later `===` lines stay in the third section.
///
/// # Panics
///
/// Panics with a descriptive message if the file contains fewer than two
/// separator lines (i.e. fewer than three sections).
pub fn parse_fixture(contents: &str) -> Case {
    let mut sections: Vec<String> = vec![String::new()];
    for line in contents.lines() {
        if sections.len() < 3 && is_separator(line) {
            sections.push(String::new());
        } else {
            let current = sections.last_mut().unwrap();
            current.push_str(line);
            current.push('\n');
        }
    }

    assert!(
        sections.len() == 3,
        "expected a corpus fixture with at least 2 `===` separator lines \
         (name, source, expected), found {} section(s)",
        sections.len()
    );

    Case {
        name: dedent_trim(&sections[0]),
        source: dedent_trim(&sections[1]),
        expected: dedent_trim(&sections[2]),
    }
}

/// Parses `source` starting from `rule`. Panics with the pest error on a parse
/// failure, or if the rule does not consume the entire source.
pub fn parse_source(rule: Rule, source: &str) -> Ast {
    let pair = NewtypeParser::parse(rule, source)
        .unwrap_or_else(|e| panic!("failed to parse as {:?}:\n{}", rule, e))
        .next()
        .unwrap_or_else(|| panic!("rule {:?} matched no pairs for source:\n{}", rule, source));

    pretty_assertions::assert_eq!(
        pair.as_span().as_str(),
        source,
        "rule {:?} did not consume the entire source",
        rule
    );

    parser::parse(pair)
}

/// Parses and simplifies `source`, then renders it to TypeScript.
pub fn render(rule: Rule, source: &str) -> String {
    parse_source(rule, source)
        .simplify()
        .render_pretty_ts(RENDER_WIDTH)
}

/// Reads the fixture at `path`, renders its source with `rule`, and asserts the
/// output matches the expected (TypeScript) section. Intended to be called from
/// a generated `#[test]` function, so a mismatch panics with a readable diff.
pub fn run_case(rule: Rule, path: &Path) {
    let contents = read_fixture(path);
    let case = parse_fixture(&contents);
    let actual = render(rule, &case.source);

    pretty_assertions::assert_eq!(case.expected, actual.trim());
}

/// Reads the fixture at `path` and asserts that its two newtype snippets
/// simplify to the same AST (compared via their s-expression form, exactly as
/// the inline `assert_expr_eq!` macro does). Intended to be called from a
/// generated `#[test]` function.
pub fn run_equivalence_case(rule: Rule, path: &Path) {
    let contents = read_fixture(path);
    let case = parse_fixture(&contents);

    let lhs = parse_source(rule, &case.source).simplify();
    let rhs = parse_source(rule, &case.expected).simplify();

    pretty_assertions::assert_eq!(
        lhs.to_sexp().unwrap(),
        rhs.to_sexp().unwrap(),
        "the two snippets are not equivalent after simplification"
    );
}

fn read_fixture(path: &Path) -> String {
    std::fs::read_to_string(path)
        .unwrap_or_else(|e| panic!("failed to read fixture {}: {}", path.display(), e))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn splits_three_sections() {
        let case = parse_fixture("Name\n\n=======\n\ntype A as 1\n\n=======\n\ntype A = 1;\n");
        assert_eq!(case.name, "Name");
        assert_eq!(case.source, "type A as 1");
        assert_eq!(case.expected, "type A = 1;");
    }

    #[test]
    #[should_panic(expected = "at least 2 `===` separator")]
    fn rejects_missing_section() {
        parse_fixture("Name\n\n=======\n\ntype A as 1\n");
    }

    #[test]
    fn separator_requires_three_equals() {
        assert!(is_separator("==="));
        assert!(is_separator("  ======= "));
        assert!(!is_separator("=="));
        assert!(!is_separator("= = ="));
        assert!(!is_separator("type A = 1"));
    }

    #[test]
    fn extra_separators_stay_in_expected_section() {
        // A `===` line inside the expected output must not start a 4th section.
        let case = parse_fixture("Name\n=======\nsrc\n=======\nline1\n=======\nline2\n");
        assert_eq!(case.source, "src");
        assert_eq!(case.expected, "line1\n=======\nline2");
    }

    #[test]
    fn dedents_common_indentation() {
        let case = parse_fixture(
            "Name\n=======\n    if a then\n        b\n    end\n=======\n    x\n",
        );
        assert_eq!(case.source, "if a then\n    b\nend");
        assert_eq!(case.expected, "x");
    }
}
