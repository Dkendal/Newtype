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

- [x] **Optional property modifier** `{ a?: T }` — grammar/parser/printer now
  accept the postfix `?` on object-type properties (the assignability engine
  already modelled `ObjectProperty.optional`). _Tests:_
  `tests/corpus/typescript/object_literal/optional_modifier_postfix.txt`.

- [x] **Bare `keyof` operand** — `keyof T` is now an `expr_prefix` (not only the
  parenthesised `keyof(T)` builtin), so it parses anywhere an operand is
  expected, e.g. `assert keyof X <: …`. Precedence sits just above `pipe`:
  `keyof A[]` is `keyof (A[])`, `keyof A | B` is `(keyof A) | B`
  (`src/grammar.pest`, `src/parser/pratt.rs`, `src/parser.rs`). _Test:_
  `tests/corpus/typescript/expr/keyof_prefix.txt`.
  - [ ] _Follow-up:_ `keyof`'s **assignability** is still the `Both` placeholder
    (`assignability.rs`), so `keyof { a: 1, b: 2 } <: string` is indeterminate
    rather than `true`. Needs a keyof evaluator (see the mapped-type plan).

- [x] **`readonly` arrays/tuples** — `readonly T[]` / `readonly [A, B]` now parse
  via a new `Ast::Readonly` wrapper (`expr_prefix`, rejected on non-array/tuple
  operands as TypeScript does). Assignability is one-directional — a mutable
  array/tuple is assignable to a `readonly` one but not the reverse
  (`src/ast/assignability.rs`), verified against `tsgo --strict`. _Tests:_
  `tests/corpus/typescript/expr/readonly_array.txt`, `readonly_tuple.txt`, and
  `assignability_tests` readonly cases. (`readonly` on object properties was
  already supported.)

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

## Assignability engine (`is_assignable_to`)

- [x] **Implemented the assignability engine** — renamed `is_subtype` →
  `is_assignable_to` and the module `src/ast/subtype.rs` →
  `src/ast/assignability.rs` (the relation is TypeScript *assignability*, not
  strict subtyping). All previously-`todo!()` left-hand-side variants now have
  real structural logic mirroring the TS checker
  (`typescript-go/internal/checker/relater.go`) and the `assignmentCompat*`
  baselines. _Tests:_ `assignability_tests::is_assignable_to_extended` in
  `tests/ast.rs` (71 cases). The `pending::subtype_engine::*` totality stubs
  were removed.
  - [x] `Array` LHS — covariant element relation; arrays are objects (`<:`
    `{}`/`object`/`Object`); not assignable to a fixed-arity tuple.
  - [x] `Tuple` LHS — element-wise, same arity; tuple-to-array (`[A,B] <: T[]`);
    empty tuple assignable to any array; tuples are objects.
  - [x] `TypeLiteral` (non-empty) LHS — structural width/depth subtyping;
    optional target properties may be absent; index/computed keys → `Both`.
  - [x] Set operations — `UnionType` LHS (every member) / RHS (some member);
    `IntersectionType` LHS (some member, or merged object shape) / RHS (every
    member).
  - [x] `Access`, `ApplyGeneric`, `Builtin` (`keyof`), `Path`, and bare `Ident`
    LHS — these are unresolvable references (no type environment at this stage,
    since `let`-bindings are substituted upstream), so they are treated as
    **indeterminate** (`ExtendsResult::Both`), which `unquote` lowers to the
    union of both conditional branches. _Design note:_ `Both` is the sound
    over-approximation (it keeps both branches and matches the existing `any`
    precedent); a follow-up could add an `Indeterminate`/deferred variant that
    re-emits the conditional verbatim for `tsc` to resolve.
  - [x] **Implicit intersection reduction** — contradictory primitive
    intersections now reduce to `never` (`string & number`, `1 & 2`, …) in
    `intersection_source_assignable`, and a `never`-typed LHS makes an
    assignability assertion hold (`src/test_harness.rs` maps a `Never` relation
    leaf to `True` before negation), so `assert string & number <: string`
    passes. Inhabited intersections (`1 & number`, `string & 'a'`) are not
    reduced. _Tests:_ `assignability_tests` intersection rows, `test_harness`
    never cases.
  - [x] **keyof / mapped types / index signatures** — `keyof O` over an object
    literal evaluates to its key union; `{ [K in T]: V }` mapped types expand to
    an object literal over a known key set (incl. `keyof O` and homomorphic
    `O[K]` bodies); `string`/`number` index signatures relate structurally. All
    verified against `tsgo --strict`; unenumerable cases stay `Both` (sound).
  - _Known limitations (deferred, not yet handled):_
    - [ ] **keyof of non-objects** — `keyof T[]` / `keyof <primitive>` stay
      `Both` (e.g. `keyof string[] <: string` is indeterminate, not `true`).
    - [x] **Object index signatures** — `type_literal_assignable_to`
      (`src/ast/assignability.rs`) now relates `string`/`number` index
      signatures structurally via a key classifier
      (`Named`/`StringIndex`/`NumberIndex`/`Other`), matching `tsgo --strict`.
      Following TS, a source index signature does NOT supply named properties
      (`{ [k in string]: number } <: { a: number }` is `False`, not `True` as a
      stale TODO premise suggested); named props must satisfy a target string
      index; a target number index constrains only numeric-named props; and a
      `number` index relates to a `string` index (and vice versa) through the
      shared value-type check. Any key over a non-`string`/`number` iterable, a
      computed key, or a remapped index stays `Both` (conservative). _Tests:_
      `assignability_tests::is_assignable_to_extended` index-signature rows in
      `tests/ast.rs`.

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
