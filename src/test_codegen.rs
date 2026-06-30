//! Emit TypeScript type-level assertions from `unittest` blocks.
//!
//! When the compiler is run with `--generate-tests`, each `assert <claim>` inside
//! a `unittest` is lowered to a `type` alias whose body wraps the claim in the
//! `Assert<…>` helper. Because the helper only accepts `true`, the emitted
//! TypeScript fails to type-check exactly when the claim does not hold — so the
//! TypeScript compiler itself becomes the test runner.
//!
//! The relational operators map onto a small set of helper types:
//!
//! | claim                | emitted body                  |
//! |----------------------|-------------------------------|
//! | `A <: B`             | `Extends<A, B>`               |
//! | `not (A <: B)`       | `Not<Extends<A, B>>`          |
//! | `A == B` / `A === B` | `Equals<A, B>`                |
//! | `A != B` / `A !== B` | `Not<Equals<A, B>>`           |
//! | `X and Y`            | `And<…X, …Y>`                 |
//! | `X or Y`             | `Or<…X, …Y>`                  |
//!
//! [`expand`] rewrites a program, replacing each `unittest` with the generated
//! aliases (in source position) and collecting the set of helpers referenced.
//! [`render_helpers`] renders only that set — closed over dependencies, since
//! `Not` is defined in terms of `Equals` — inside a `// START/END Newtype Test
//! Helpers` comment fence, which the caller prepends to the emitted file.

use std::collections::BTreeSet;
use std::rc::Rc;

use crate::ast::{
    ApplyGeneric, Ast, ExtendsInfixOp, ExtendsPrefixOp, Ident, InfixOp, PrefixOp, Program, Span,
    TypeAlias, UnitTest,
};

/// A TypeScript helper type the generated assertions may reference.
///
/// Declaration order is the canonical emission order in the fence (so the set,
/// being a [`BTreeSet`], renders deterministically).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Helper {
    Assert,
    Refute,
    Equals,
    Extends,
    Not,
    And,
    Or,
}

impl Helper {
    /// The TypeScript declaration emitted into the helper fence.
    fn definition(self) -> &'static str {
        match self {
            Helper::Assert => "type Assert<T extends true> = T;",
            Helper::Refute => "type Refute<T extends false> = T;",
            Helper::Equals => {
                "type Equals<A, B> =\n  \
                 (<T>() => T extends A ? 1 : 2) extends (<T>() => T extends B ? 1 : 2)\n  \
                 ? true : false;"
            }
            Helper::Extends => "type Extends<A, B> = [A] extends [B] ? true : false;",
            Helper::Not => "type Not<A> = Equals<A, false>;",
            Helper::And => "type And<A, B> = [A, B] extends [true, true] ? true : false;",
            Helper::Or => {
                "type Or<A, B> = [A] extends [true] ? true : [B] extends [true] ? true : false;"
            }
        }
    }

    /// Helpers this one references in its own body, so the fence stays closed
    /// (e.g. emitting `Not` requires `Equals`).
    fn dependencies(self) -> &'static [Helper] {
        match self {
            Helper::Not => &[Helper::Equals],
            _ => &[],
        }
    }
}

/// The result of [`expand`]: the rewritten program plus the helpers its
/// generated assertions reference.
pub struct Expansion {
    pub ast: Ast,
    pub helpers: BTreeSet<Helper>,
    /// Generated type name -> 1-based source line of the originating `assert`,
    /// in generation (body) order. Consumed by [`attach_comments`].
    pub comments: Vec<(String, usize)>,
}

/// Rewrite `program`, replacing each `unittest` with the `type` aliases its
/// `assert`s lower to. Non-`unittest` statements are preserved verbatim, in
/// order, so the generated aliases land where the `unittest` was.
///
/// `source` is the original program text, used to compute the source line of
/// each originating `assert` for [`Expansion::comments`].
pub fn expand(program: &Ast, source: &str) -> Expansion {
    use std::collections::HashMap;

    let mut helpers = BTreeSet::new();
    let mut comments = Vec::new();
    // Disambiguate unittests whose descriptions slugify to the same identifier so
    // generated type names stay globally unique (duplicate `type` aliases would
    // otherwise be invalid TypeScript). Slugs never contain `__` — `slugify`
    // collapses each run of separators to a single `_` — so the `__N` suffix
    // can't collide with any real slug.
    let mut slug_counts: HashMap<String, usize> = HashMap::new();

    let statements = match program {
        Ast::Program(Program { statements, .. }) => statements.as_slice(),
        other => std::slice::from_ref(other),
    };

    let mut out = Vec::with_capacity(statements.len());
    for statement in statements {
        match as_unittest(statement) {
            Some(unittest) => {
                let base = slugify(&unittest.name);
                let n = {
                    let count = slug_counts.entry(base.clone()).or_insert(0);
                    *count += 1;
                    *count
                };
                let slug = if n == 1 { base } else { format!("{base}__{n}") };
                for alias in generate_aliases(unittest, &slug, source, &mut helpers, &mut comments)
                {
                    out.push(Ast::Statement(Rc::new(alias)));
                }
            }
            None => out.push(statement.clone()),
        }
    }

    close_dependencies(&mut helpers);

    let ast = Ast::Program(Program {
        statements: out,
        span: program.as_span(),
    });

    Expansion {
        ast,
        helpers,
        comments,
    }
}

/// Insert a `/** @newtype line:N */` comment on its own line directly above each
/// generated `type <name> = …` declaration, where `N` is the 1-based source line
/// of the originating `assert`. `comments` maps generated type names to their
/// source lines (as produced in [`Expansion::comments`]).
///
/// Only lines that begin with `type ` whose identifier is a known generated
/// name receive a comment; every other line (the helper fence, interfaces,
/// preserved user aliases, wrapped continuation lines) passes through verbatim.
/// Dependency-free: no `regex` crate.
pub fn attach_comments(body: &str, comments: &[(String, usize)]) -> String {
    use std::collections::HashMap;

    let lookup: HashMap<&str, usize> = comments
        .iter()
        .map(|(name, line)| (name.as_str(), *line))
        .collect();

    // Preserve the original trailing newline: `split('\n')` on text ending in
    // '\n' yields a final empty segment, which re-joining with '\n' restores.
    let mut out: Vec<String> = Vec::new();
    for line in body.split('\n') {
        if let Some(rest) = line.strip_prefix("type ") {
            let name: String = rest
                .chars()
                .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
                .collect();
            if let Some(&n) = lookup.get(name.as_str()) {
                out.push(format!("/** @newtype line:{n} */"));
            }
        }
        out.push(line.to_string());
    }

    out.join("\n")
}

/// Render the needed helpers inside the `// START/END Newtype Test Helpers`
/// comment fence. Returns an empty string when no helpers are needed (no tests
/// were generated), so the caller can skip prepending anything.
pub fn render_helpers(helpers: &BTreeSet<Helper>) -> String {
    if helpers.is_empty() {
        return String::new();
    }

    let defs: Vec<&str> = helpers.iter().map(|helper| helper.definition()).collect();

    format!(
        "// START Newtype Test Helpers\n\n{}\n\n// END Newtype Test Helpers\n",
        defs.join("\n\n")
    )
}

/// Generate one `type` alias per `assert` in `unittest`. The alias name is
/// `_newtype_test__<slug>_<index>`, where `<slug>` is the (already
/// disambiguated) test-description slug and `<index>` is the 0-based position of
/// the assert in the block.
fn generate_aliases(
    unittest: &UnitTest,
    slug: &str,
    source: &str,
    helpers: &mut BTreeSet<Helper>,
    comments: &mut Vec<(String, usize)>,
) -> Vec<Ast> {
    unittest
        .body
        .iter()
        .filter_map(|statement| match statement {
            Ast::Assert(assert) => Some(assert),
            _ => None,
        })
        .enumerate()
        .map(|(index, assert)| {
            let body = wrap_assert(&assert.claim, helpers);

            let name = format!("_newtype_test__{slug}_{index}");
            // 1-based line of the *claim*, not the `assert` keyword: the harness
            // reports failures at the claim position (test_harness::claim_span),
            // so the acceptance runner can only line up newtype's `--> LINE` with
            // tsgo's error if this comment points at the same line. `span.start()`
            // is a char boundary, so byte-counting '\n' before it is exact.
            let line = source[..assert.claim.as_span().start()]
                .bytes()
                .filter(|&b| b == b'\n')
                .count()
                + 1;
            comments.push((name.clone(), line));

            Ast::TypeAlias(TypeAlias {
                export: false,
                name: Ident { name, span: span() },
                params: Vec::new(),
                body: Rc::new(body),
                span: span(),
            })
        })
        .collect()
}

/// Wrap a lowered claim in `Assert<…>`.
fn wrap_assert(claim: &Ast, helpers: &mut BTreeSet<Helper>) -> Ast {
    helpers.insert(Helper::Assert);
    apply("Assert", vec![lower(claim, helpers)])
}

/// Lower a relational claim to its TypeScript helper expression, recording the
/// helpers it uses. Non-relational nodes (a bare `true`/`false`, or anything the
/// assert harness wouldn't accept) pass through unchanged.
fn lower(claim: &Ast, helpers: &mut BTreeSet<Helper>) -> Ast {
    match claim {
        Ast::ExtendsPrefixOp(ExtendsPrefixOp {
            op: PrefixOp::Not,
            value,
            ..
        }) => {
            helpers.insert(Helper::Not);
            apply("Not", vec![lower(value, helpers)])
        }

        Ast::ExtendsInfixOp(ExtendsInfixOp { lhs, op, rhs, .. }) => match op {
            InfixOp::Extends => {
                helpers.insert(Helper::Extends);
                apply("Extends", vec![(**lhs).clone(), (**rhs).clone()])
            }
            InfixOp::NotExtends => {
                helpers.insert(Helper::Not);
                helpers.insert(Helper::Extends);
                apply(
                    "Not",
                    vec![apply("Extends", vec![(**lhs).clone(), (**rhs).clone()])],
                )
            }
            InfixOp::Equals | InfixOp::StrictEquals => {
                helpers.insert(Helper::Equals);
                apply("Equals", vec![(**lhs).clone(), (**rhs).clone()])
            }
            InfixOp::NotEquals | InfixOp::StrictNotEquals => {
                helpers.insert(Helper::Not);
                helpers.insert(Helper::Equals);
                apply(
                    "Not",
                    vec![apply("Equals", vec![(**lhs).clone(), (**rhs).clone()])],
                )
            }
            InfixOp::And => {
                helpers.insert(Helper::And);
                apply("And", vec![lower(lhs, helpers), lower(rhs, helpers)])
            }
            InfixOp::Or => {
                helpers.insert(Helper::Or);
                apply("Or", vec![lower(lhs, helpers), lower(rhs, helpers)])
            }
        },

        Ast::TrueKeyword(_) => Ast::TrueKeyword(span()),
        Ast::FalseKeyword(_) => Ast::FalseKeyword(span()),
        other => other.clone(),
    }
}

/// Build `Name<args…>`.
fn apply(name: &str, args: Vec<Ast>) -> Ast {
    Ast::ApplyGeneric(ApplyGeneric {
        receiver: Rc::new(Ast::Ident(Ident {
            name: name.to_string(),
            span: span(),
        })),
        args,
        span: span(),
    })
}

/// Add the transitive helper dependencies of everything already in `helpers`.
fn close_dependencies(helpers: &mut BTreeSet<Helper>) {
    let mut queue: Vec<Helper> = helpers.iter().copied().collect();
    while let Some(helper) = queue.pop() {
        for &dependency in helper.dependencies() {
            if helpers.insert(dependency) {
                queue.push(dependency);
            }
        }
    }
}

/// Turn a test description into a `snake_case` identifier fragment: lowercased,
/// with each run of non-alphanumeric characters collapsed to a single `_` and
/// leading/trailing underscores trimmed.
fn slugify(name: &str) -> String {
    let mut out = String::with_capacity(name.len());
    let mut at_separator = false;

    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() {
            out.push(ch.to_ascii_lowercase());
            at_separator = false;
        } else if !at_separator {
            out.push('_');
            at_separator = true;
        }
    }

    out.trim_matches('_').to_string()
}

/// Unwrap a top-level statement to the `unittest` it contains, if any.
fn as_unittest(node: &Ast) -> Option<&UnitTest> {
    match node {
        Ast::UnitTest(unittest) => Some(unittest),
        Ast::Statement(inner) => as_unittest(inner),
        _ => None,
    }
}

/// A placeholder span for synthesized nodes. These nodes are only rendered, not
/// diagnosed, so their span is never consulted.
fn span() -> Span {
    Span::new(0, 0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_newtype_program;
    use crate::typescript::Pretty;

    /// Parse + simplify `src`, expand its tests, and return the full emitted
    /// TypeScript (helper fence + body), matching what the compiler writes.
    fn generate(src: &str) -> String {
        let program = parse_newtype_program(src).unwrap().simplify();
        let expansion = expand(&program, src);
        let header = render_helpers(&expansion.helpers);
        let body = expansion.ast.render_pretty_ts(120);
        if header.is_empty() {
            body
        } else {
            format!("{header}\n{body}")
        }
    }

    #[test]
    fn slugify_collapses_non_alphanumerics() {
        assert_eq!(slugify("a duck is a bird"), "a_duck_is_a_bird");
        assert_eq!(slugify("  Hello, World!  "), "hello_world");
        assert_eq!(slugify("string <: unknown"), "string_unknown");
    }

    #[test]
    fn extends_and_not_emit_minimal_helpers() {
        let src = "interface Duck { fly: () => void, quack: () => void }\n\
            interface Bird { fly: () => void }\n\
            unittest \"a duck is a bird\" do\n\
            \x20 assert Duck <: Bird\n\
            \x20 assert not(Bird <: Duck)\n\
            end";

        let out = generate(src);

        // The minimal helper set: `Not` pulls in `Equals`, but `Refute` is never
        // used by generation and so is omitted.
        assert!(out.contains("// START Newtype Test Helpers"));
        assert!(out.contains("type Assert<T extends true> = T;"));
        assert!(out.contains("type Equals<A, B> ="));
        assert!(out.contains("type Extends<A, B> = [A] extends [B] ? true : false;"));
        assert!(out.contains("type Not<A> = Equals<A, false>;"));
        assert!(!out.contains("Refute"));
        assert!(!out.contains("type And"));
        assert!(!out.contains("type Or"));

        assert!(
            out.contains("type _newtype_test__a_duck_is_a_bird_0 = Assert<Extends<Duck, Bird>>;")
        );
        assert!(out.contains(
            "type _newtype_test__a_duck_is_a_bird_1 = Assert<Not<Extends<Bird, Duck>>>;"
        ));

        // Interfaces are preserved in source order, before the generated tests.
        let duck = out.find("interface Duck").unwrap();
        let test0 = out.find("_newtype_test__a_duck_is_a_bird_0").unwrap();
        assert!(duck < test0);
    }

    #[test]
    fn equality_maps_to_equals_helper() {
        let out = generate("unittest \"eq\" do\n  assert 1 == 1\nend");
        assert!(out.contains("type _newtype_test__eq_0 = Assert<Equals<1, 1>>;"));
        assert!(out.contains("type Equals<A, B> ="));
        assert!(!out.contains("type Extends"));
    }

    #[test]
    fn conjunction_uses_and_helper() {
        let out = generate(
            "unittest \"both\" do\n  assert (string <: unknown) and (number <: unknown)\nend",
        );
        assert!(out.contains(
            "type _newtype_test__both_0 = Assert<And<Extends<string, unknown>, Extends<number, unknown>>>;"
        ));
        assert!(out.contains("type And<A, B>"));
    }

    #[test]
    fn program_without_unittests_emits_no_fence() {
        let out = generate("type Foo as 1");
        assert!(!out.contains("Newtype Test Helpers"));
        assert!(out.contains("type Foo = 1;"));
    }

    #[test]
    fn attach_comments_annotates_generated_aliases_with_source_lines() {
        // `assert`s sit on source lines 2 and 3 (1-based).
        let src = "unittest \"a duck is a bird\" do\n\
            \x20 assert string <: unknown\n\
            \x20 assert number <: unknown\n\
            end";

        let program = parse_newtype_program(src).unwrap().simplify();
        let expansion = expand(&program, src);
        let body = expansion.ast.render_pretty_ts(120);
        let annotated = attach_comments(&body, &expansion.comments);

        // Each generated alias is immediately preceded by its source-line comment.
        assert!(annotated.contains(
            "/** @newtype line:2 */\ntype _newtype_test__a_duck_is_a_bird_0 = Assert<Extends<string, unknown>>;"
        ));
        assert!(annotated.contains(
            "/** @newtype line:3 */\ntype _newtype_test__a_duck_is_a_bird_1 = Assert<Extends<number, unknown>>;"
        ));
        // Exactly one comment per generated alias.
        assert_eq!(annotated.matches("/** @newtype line:").count(), 2);
    }

    #[test]
    fn comment_line_tracks_claim_not_assert_keyword() {
        // The `assert` keyword is on line 2 but its claim is on line 3. The
        // comment must point at the claim (line 3) to match the harness `--> 3`,
        // so the acceptance runner lines newtype's report up with tsgo's errors.
        let src = "unittest \"t\" do\n  assert\n    number <: string\nend";
        let program = parse_newtype_program(src).unwrap().simplify();
        let expansion = expand(&program, src);
        assert_eq!(
            expansion.comments,
            vec![("_newtype_test__t_0".to_string(), 3)]
        );
    }

    #[test]
    fn duplicate_unittest_names_get_unique_type_names() {
        // Two unittests slugifying to the same base must not emit colliding
        // identifiers (which would be invalid TypeScript / duplicate decls).
        let src = "unittest \"dup\" do\n  assert 1 <: number\nend\n\
            unittest \"dup\" do\n  assert 2 <: number\nend";
        let program = parse_newtype_program(src).unwrap().simplify();
        let expansion = expand(&program, src);
        let names: Vec<&str> = expansion.comments.iter().map(|(n, _)| n.as_str()).collect();
        assert_eq!(
            names,
            vec!["_newtype_test__dup_0", "_newtype_test__dup__2_0"]
        );
    }

    #[test]
    fn attach_comments_leaves_helpers_interfaces_and_user_aliases_untouched() {
        let comments = vec![("_newtype_test__t_0".to_string(), 7usize)];
        let body = "// START Newtype Test Helpers\n\
            type Assert<T extends true> = T;\n\
            type Equals<A, B> =\n  \
            (<T>() => T extends A ? 1 : 2) extends (<T>() => T extends B ? 1 : 2)\n  \
            ? true : false;\n\
            // END Newtype Test Helpers\n\
            interface Bird { fly: () => void }\n\
            type Foo = 1;\n\
            type _newtype_test__t_0 = Assert<Extends<string, unknown>>;\n";

        let out = attach_comments(body, &comments);

        // Helper fence and its multi-line defs are never annotated.
        assert!(!out.contains("/** @newtype line:7 */\ntype Assert"));
        assert!(!out.contains("/** @newtype line:7 */\ntype Equals"));
        // Interfaces and preserved user aliases are untouched.
        assert!(out.contains("interface Bird { fly: () => void }"));
        assert!(!out.contains("/** @newtype */\ntype Foo"));
        assert!(out.contains("type Foo = 1;"));
        // Only the generated alias gets the comment.
        assert!(out.contains(
            "/** @newtype line:7 */\ntype _newtype_test__t_0 = Assert<Extends<string, unknown>>;"
        ));
        assert_eq!(out.matches("/** @newtype line:").count(), 1);
        // Trailing newline preserved.
        assert!(out.ends_with('\n'));
    }
}
