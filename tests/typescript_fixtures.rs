//! Corpus tests for newtype-source -> TypeScript-output rendering.
//!
//! Each `#[typescript_tests(...)]` module generates one `#[test]` per fixture
//! file in its `dir`. A fixture has three `===`-separated sections: a name, the
//! newtype source, and the expected TypeScript. The second positional argument
//! selects the grammar `Rule` to parse from, so different grammar units
//! (whole programs, bare expressions, if-expressions, ...) can each have their
//! own corpus. Fixtures live under `tests/corpus/typescript/<rule>/`.
//!
//! See `newtype::corpus` for the fixture format and runner.

use newtype_macros_lib::typescript_tests;

#[typescript_tests(
    newtype::parser::Rule,
    "program",
    dir = "tests/corpus/typescript/program",
    recursive = true
)]
#[cfg(test)]
mod program {}

#[typescript_tests(
    newtype::parser::Rule,
    "expr",
    dir = "tests/corpus/typescript/expr",
    recursive = true
)]
#[cfg(test)]
mod expr {}

#[typescript_tests(
    newtype::parser::Rule,
    "if_expr",
    dir = "tests/corpus/typescript/if_expr",
    recursive = true
)]
#[cfg(test)]
mod if_expr {}

#[typescript_tests(
    newtype::parser::Rule,
    "type_alias",
    dir = "tests/corpus/typescript/type_alias",
    recursive = true
)]
#[cfg(test)]
mod type_alias {}

#[typescript_tests(
    newtype::parser::Rule,
    "interface",
    dir = "tests/corpus/typescript/interface",
    recursive = true
)]
#[cfg(test)]
mod interface {}

#[typescript_tests(
    newtype::parser::Rule,
    "tuple",
    dir = "tests/corpus/typescript/tuple",
    recursive = true
)]
#[cfg(test)]
mod tuple {}

#[typescript_tests(
    newtype::parser::Rule,
    "object_literal",
    dir = "tests/corpus/typescript/object_literal",
    recursive = true
)]
#[cfg(test)]
mod object_literal {}

#[typescript_tests(
    newtype::parser::Rule,
    "map_expr",
    dir = "tests/corpus/typescript/map_expr",
    recursive = true
)]
#[cfg(test)]
mod map_expr {}
