use pest_test_gen::pest_tests;

#[pest_tests(
    newtype::parser::NewtypeParser,
    newtype::parser::Rule,
    "program",
    dir = "tests/pest/type_alias",
    recursive = true,
    lazy_static = true
)]
#[cfg(test)]
mod type_alias {}

#[pest_tests(
    newtype::parser::NewtypeParser,
    newtype::parser::Rule,
    "program",
    dir = "tests/pest/interface",
    recursive = true,
    lazy_static = true
)]
#[cfg(test)]
mod interface {}
