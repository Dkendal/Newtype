//! Build script.
//!
//! The corpus-test proc macros (`typescript_tests` / `equivalent_tests`) walk
//! `tests/corpus` at compile time and generate one test per fixture file. Cargo
//! does not otherwise know the generated set depends on that directory, so a new
//! or deleted fixture would be missed until something else forced a rebuild.
//! Declaring the directory here makes cargo re-run this script — and recompile
//! the dependent test targets — whenever the corpus changes.
fn main() {
    println!("cargo:rerun-if-changed=tests/corpus");
}
