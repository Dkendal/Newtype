//! Corpus tests for newtype-expression *equivalence*.
//!
//! Each `#[equivalent_tests(...)]` module generates one `#[test]` per fixture
//! file in its `dir`. A fixture has three `===`-separated sections: a name and
//! two newtype snippets that must simplify to the same AST (the corpus form of
//! the inline `assert_expr_eq!` macro). The second positional argument selects
//! the grammar `Rule` both snippets are parsed from, so equivalence can be
//! asserted for whole programs, bare expressions, and other grammar units.
//! Fixtures live under `tests/corpus/newtype/<rule>/`.
//!
//! See `newtype::corpus` for the fixture format and runner.

use newtype_macros_lib::equivalent_tests;

#[equivalent_tests(newtype::parser::Rule, "expr", dir = "tests/corpus/newtype/expr", recursive = true)]
#[cfg(test)]
mod expr {}

#[equivalent_tests(newtype::parser::Rule, "program", dir = "tests/corpus/newtype/program", recursive = true)]
#[cfg(test)]
mod program {}
