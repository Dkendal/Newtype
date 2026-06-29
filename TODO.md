# TODO

Open issues discovered while building the corpus test suites. Each item notes
its source location and the test that will go green once it's addressed.

Pending specs live in `tests/pending.rs` (all `#[ignore]`d). Run them with:

```sh
cargo test --test pending -- --ignored
```

When you fix an item, its test should pass — then delete the `#[ignore]` or
promote the case into `tests/corpus/`.

---

## Unimplemented language features

- [x] **Interface `extends` clause** — `parse_interface` now reads the `#extends`
  ident (`src/parser.rs`) instead of hardcoding `None`, so
  `interface Foo extends Bar {}` renders correctly. _Test:_
  `pending::interface_extends` (passing, un-ignored).

- [x] **Namespace imports** (`import * as Foo from :a`) — `parse_import_statement`
  now lowers `namespace_import` to `ImportClause::Namespace` (`src/parser.rs`),
  rendering `import type * as Foo from 'a';`. _Test:_
  `pending::namespace_import` (passing, un-ignored).

- [x] **`map_expr` modifiers** (`readonly` / optional `?` / `as` remap) —
  `parse_map_expr` now extracts the `readonly`/`optional` modifier tags and the
  `remap_clause` (`src/parser.rs`). Also fixed a printer bug that dropped the
  space after `readonly` (`src/ast/pretty.rs`). _Test:_
  `pending::map_expr_modifiers` (passing, un-ignored).

- [x] **Equality extends operators** `=`, `!=`, `==`, `!==` — `expand_to_extends`
  (`src/ast/if_expr.rs`) now lowers these via mutual assignability:
  `a = b` → `(a <: b) and (b <: a)`; `a != b` → `(a </: b) or (b </: a)`. Strict
  forms (`==`/`!==`) map identically to the loose forms for now. Also fixed a
  grammar ordering bug where `=`/`!=` shadowed `==`/`!==` in the `extends_infix`
  ordered choice (`src/grammar.pest`). _Test:_ `pending::equality_operators`
  (passing with exact-output assertions, un-ignored).
  - [ ] _Follow-up:_ make the **strict** forms (`==`/`!==`) compile to the
    function-identity trick
    `(<T>() => T extends A ? 1 : 2) extends (<T>() => T extends B ? 1 : 2)` for
    true type-identity (distinct from the loose mutual-assignability forms), and
    tighten `pending::equality_operators::strict_*` to the new exact output.

- [ ] **Macro calls** (`name!(...)`) — `Ast::MacroCall` is `todo!()`
  (`src/runtime.rs:28`) and `unreachable!()` in the renderer
  (`src/ast/pretty.rs:388`). `unquote!` is intended to evaluate its argument
  (`unquote!(1)` → `1`); `dbg!` / `assert_equal!` outputs are undecided. _Tests:_
  `pending::macro_calls`, and `tests/parser.rs` `unquote::evaluates_expression`.

- [ ] **Double negation** `not (not (a <: b))` — the parser rejects it ("not may
  only be used with an extends expression"); `not` of `not` is unsupported.
  Decide whether to allow it (simplifying the double negation away).

## Subtype engine (`is_subtype`)

`src/ast/subtype.rs` has `todo!()` for several left-hand-side AST variants, so
asking whether one is a subtype of anything panics. Existing coverage in
`tests/ast.rs::is_subtype_tests` only exercises primitive/literal/top/bottom
LHS types. _Tests:_ `pending::subtype_engine::*`.

- [ ] `Access` LHS — `src/ast/subtype.rs:36`
- [ ] `ApplyGeneric` LHS — `src/ast/subtype.rs:38`
- [ ] `Array` LHS — `src/ast/subtype.rs:40`
- [ ] `Builtin` LHS — `src/ast/subtype.rs:42`
- [ ] `Path` LHS — `src/ast/subtype.rs:44`
- [ ] `TypeLiteral` (non-empty) LHS — `src/ast/subtype.rs:54`
- [ ] `Ident` LHS — `src/ast/subtype.rs:78`
- [ ] `Tuple` LHS — `src/ast/subtype.rs:81`

## Bugs

- [ ] **Dot-then-indexed access** `A.b[C]` panics in the renderer ("rhs of dot
  access should be an ident", `src/ast/pretty.rs:219`) instead of chaining to
  `A['b'][C]`. _Test:_ `pending::known_bugs::dot_then_indexed_access`.

- [ ] **`unittest` statement emits a stray `;`** — `Ast::UnitTest` renders to
  `D::nil()` (`src/ast/pretty.rs`), but the wrapping `Ast::Statement` still
  appends `;`, so a program containing a unittest renders a stray `;` line
  instead of nothing. _Tests:_ `pending::unittest_statement`, and
  `tests/parser.rs` `unittest_statement::typescript_no_output`.

- [x] **Intersection-of-unions dropped parens** — the `IntersectionType` printer
  did not parenthesise union members, so `(A | B) & (C | D)` rendered as
  `A | B & C | D` (a different type, since `&` binds tighter). Fixed in
  `src/ast/pretty.rs`; locked in by
  `tests/corpus/typescript/expr/intersection_of_unions.txt` and
  `union_in_intersection.txt`.

## Dead / unreachable code

- [ ] **`Ast::eval()` is unused** (`src/ast.rs:228`, no callers). The evaluation
  path it gates — `Ast::MacroCall` (`src/runtime.rs:28`) and `Ast::MappedType`
  (`src/runtime.rs:50`) `todo!()`s, plus the `is_subtype` arms above — is only
  reachable through it. Either wire up evaluation (e.g. for running `unittest`
  blocks) or remove the dead code.

- [ ] **Unreachable `PrefixOp::Infer` arm** (`src/ast/if_expr.rs:46`, `todo!()`)
  — the grammar only allows `not` as a condition prefix
  (`extends_prefix = _{ not }`), so an infer prefix can never reach it. Remove
  the arm or revisit the grammar if infer-in-condition is intended.
