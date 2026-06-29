//! Corpus tests for `unittest` assertion evaluation.
//!
//! Each `#[assertion_tests(...)]` module generates one `#[test]` per fixture
//! file. A fixture has the usual three `===`-separated sections — a name, the
//! newtype source, and the expected TypeScript — but the runner does more than
//! check rendering: it also evaluates every `assert` in the program's `unittest`
//! blocks and fails if any does not hold. This gives end-to-end coverage of
//! parsing, simplification, top-level type resolution, assignability, and
//! rendering in one declarative fixture.
//!
//! See `newtype::corpus::run_assertion_case` for the runner.

use newtype_macros_lib::assertion_tests;

#[assertion_tests(newtype::parser::Rule, "program", dir = "tests/corpus/assertions", recursive = true)]
#[cfg(test)]
mod assertions {}
