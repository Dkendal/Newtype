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

- [ ] **`typeof` builtin** — `parse_builtin` only maps `Rule::keyof`; `typeof`
  hits `unreachable!()` (`src/parser.rs:363`) and `BuiltinKeyword` only has
  `Keyof` (`src/ast.rs:915`). Renderer would print `typeof <arg>` by analogy
  with `keyof`. _Test:_ `pending::typeof_builtin`.

- [ ] **Interface `extends` clause** — the grammar parses it, but
  `parse_interface` hardcodes `extends = None` (`src/parser.rs`), so it is
  silently dropped (`interface Foo extends Bar {}` renders as
  `interface Foo {}`). The renderer already supports it (`src/ast/pretty.rs:64`).
  Note: `extends_clause` allows only a single ident. _Test:_
  `pending::interface_extends`.

- [ ] **Namespace imports** (`import * as Foo from :a`) — parser lowering is
  `todo!()` (`src/parser.rs:884`, "not yet implemented"). Renderer supports
  `ImportClause::Namespace`. Expected: `import type * as Foo from 'a';`.
  _Test:_ `pending::namespace_import`.

- [ ] **`map_expr` modifiers** (`readonly` / optional `?` / `as` remap) —
  `parse_map_expr` hardcodes `readonly_mod`/`optional_mod`/`remapped_as` to
  `None` (and panics on the `readonly` prefix), so modifiers are dropped. The
  `MappedType` renderer already handles them (`src/ast/pretty.rs:333`). _Test:_
  `pending::map_expr_modifiers`.

- [ ] **Equality extends operators** `=`, `!=`, `==`, `!==` — `todo!()` in
  `expand_to_extends` (`src/ast/if_expr.rs:103-106`), so any `if`/`cond` using
  them panics during `simplify()`. **The lowering semantics are an open design
  decision** — decide what each maps to, then tighten the tests from
  "renders OK" to exact output. _Test:_ `pending::equality_operators`.

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
