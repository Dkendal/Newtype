use newtype::extends_result::ExtendsResult;

#[macro_use]
mod common;

mod assignability_tests {
    use super::*;
    use rstest::rstest;

    static TRUE: ExtendsResult = ExtendsResult::True;
    static FALSE: ExtendsResult = ExtendsResult::False;
    static BOTH: ExtendsResult = ExtendsResult::Both;
    static NEVER: ExtendsResult = ExtendsResult::Never;

    #[rstest]
    // never
    #[case("never", "any", NEVER)]
    #[case("never", "unknown", NEVER)]
    #[case("never", "{}", NEVER)]
    #[case("never", "[]", NEVER)]
    #[case("never", "string", NEVER)]
    #[case("never", "number", NEVER)]
    #[case("never", "object", NEVER)]
    #[case("never", "boolean", NEVER)]
    #[case("never", "Object", NEVER)]
    #[case("never", "1", NEVER)]
    #[case("never", "'string'", NEVER)]
    #[case("never", "true", NEVER)]
    #[case("never", "false", NEVER)]
    #[case("never", "null", NEVER)]
    #[case("never", "undefined", NEVER)]
    #[case("never", "never", NEVER)]
    // any
    #[case("any", "any", TRUE)]
    #[case("any", "unknown", TRUE)]
    #[case("any", "{}", BOTH)]
    #[case("any", "[]", BOTH)]
    #[case("any", "string", BOTH)]
    #[case("any", "number", BOTH)]
    #[case("any", "boolean", BOTH)]
    #[case("any", "object", BOTH)]
    #[case("any", "Object", BOTH)]
    #[case("any", "1", BOTH)]
    #[case("any", "'string'", BOTH)]
    #[case("any", "true", BOTH)]
    #[case("any", "false", BOTH)]
    #[case("any", "null", BOTH)]
    #[case("any", "undefined", BOTH)]
    #[case("any", "never", BOTH)]
    // unknown
    #[case("unknown", "any", TRUE)]
    #[case("unknown", "unknown", TRUE)]
    #[case("unknown", "{}", FALSE)]
    #[case("unknown", "[]", FALSE)]
    #[case("unknown", "string", FALSE)]
    #[case("unknown", "number", FALSE)]
    #[case("unknown", "boolean", FALSE)]
    #[case("unknown", "object", FALSE)]
    #[case("unknown", "Object", FALSE)]
    #[case("unknown", "1", FALSE)]
    #[case("unknown", "'string'", FALSE)]
    #[case("unknown", "true", FALSE)]
    #[case("unknown", "false", FALSE)]
    #[case("unknown", "null", FALSE)]
    #[case("unknown", "undefined", FALSE)]
    #[case("unknown", "never", FALSE)]
    // {}
    #[case("{}", "any", TRUE)]
    #[case("{}", "unknown", TRUE)]
    #[case("{}", "{}", TRUE)]
    #[case("{}", "{ x: string }", FALSE)]
    #[case("{}", "[]", FALSE)]
    #[case("{}", "[{}]", FALSE)]
    #[case("{}", "{}[]", FALSE)]
    #[case("{}", "string", FALSE)]
    #[case("{}", "number", FALSE)]
    #[case("{}", "boolean", FALSE)]
    #[case("{}", "bitint", FALSE)]
    #[case("{}", "symbol", FALSE)]
    #[case("{}", "object", TRUE)]
    #[case("{}", "Object", TRUE)]
    #[case("{}", "Function", FALSE)]
    #[case("{}", "String", FALSE)]
    #[case("{}", "1", FALSE)]
    #[case("{}", "'string'", FALSE)]
    #[case("{}", "`string${var}`", FALSE)]
    #[case("{}", "true", FALSE)]
    #[case("{}", "false", FALSE)]
    #[case("{}", "null", FALSE)]
    #[case("{}", "undefined", FALSE)]
    #[case("{}", "never", FALSE)]
    // string
    #[case("string", "any", TRUE)]
    #[case("string", "unknown", TRUE)]
    #[case("string", "{}", FALSE)]
    #[case("string", "[]", FALSE)]
    #[case("string", "string", TRUE)]
    #[case("string", "number", FALSE)]
    #[case("string", "boolean", FALSE)]
    #[case("string", "Object", TRUE)]
    #[case("string", "Function", FALSE)]
    #[case("string", "String", TRUE)]
    #[case("string", "1", FALSE)]
    #[case("string", "'string'", FALSE)]
    #[case("string", "true", FALSE)]
    #[case("string", "false", FALSE)]
    #[case("string", "null", FALSE)]
    #[case("string", "undefined", FALSE)]
    #[case("string", "never", FALSE)]
    // String
    #[case("String", "any", TRUE)]
    #[case("String", "unknown", TRUE)]
    #[case("String", "{}", FALSE)]
    #[case("String", "[]", FALSE)]
    #[case("String", "string", TRUE)]
    #[case("String", "number", FALSE)]
    #[case("String", "boolean", FALSE)]
    #[case("String", "Object", TRUE)]
    #[case("String", "Function", FALSE)]
    #[case("String", "String", TRUE)]
    #[case("String", "1", FALSE)]
    #[case("String", "'string'", FALSE)]
    #[case("String", "true", FALSE)]
    #[case("String", "false", FALSE)]
    #[case("String", "null", FALSE)]
    #[case("String", "undefined", FALSE)]
    #[case("String", "never", FALSE)]
    #[trace]
    fn is_assignable_to(#[case] a: &str, #[case] b: &str, #[case] expected: ExtendsResult) {
        assert_eq!(ast!(a).is_assignable_to(&ast!(b)), expected);
    }

    /// Structural assignability for the LHS variants that were previously
    /// `todo!()`: arrays, tuples, non-empty object literals, the opaque/
    /// unresolvable references (`Ident`, `Path`, `Access`, `ApplyGeneric`,
    /// `keyof`), and set operations (unions & intersections). Expected values
    /// mirror the TypeScript checker's assignable relation
    /// (`internal/checker/relater.go`) and the `assignmentCompat*` baselines,
    /// subject to this engine's existing conventions (e.g. primitives are not
    /// assignable to `{}`, but object/array/tuple values are).
    #[rstest]
    // --- Array LHS ---
    #[case("number[]", "number[]", TRUE)] // reflexive
    #[case("1[]", "number[]", TRUE)] // covariant element: 1 <: number
    #[case("number[]", "string[]", FALSE)]
    #[case("string[]", "number[]", FALSE)]
    #[case("never[]", "string[]", TRUE)] // never element is assignable
    #[case("string[]", "never[]", FALSE)]
    #[case("number[]", "object", TRUE)] // arrays are objects
    #[case("number[]", "{}", TRUE)]
    #[case("number[]", "Object", TRUE)]
    #[case("number[]", "[number]", FALSE)] // array is not a fixed-arity tuple
    #[case("number[]", "string", FALSE)]
    #[case("number[]", "number", FALSE)]
    #[case("number[][]", "number[][]", TRUE)] // nested arrays
    #[case("number[][]", "string[][]", FALSE)]
    // --- Tuple LHS ---
    #[case("[number, string]", "[number, string]", TRUE)] // reflexive
    #[case("[1, 'a']", "[number, string]", TRUE)] // element-wise widening
    #[case("[number]", "[string]", FALSE)]
    #[case("[number, string]", "[number]", FALSE)] // too long
    #[case("[number]", "[number, string]", FALSE)] // too short
    #[case("[number, string]", "[string, number]", FALSE)] // order matters
    #[case("[number, number]", "number[]", TRUE)] // tuple to array
    #[case("[1, 2]", "number[]", TRUE)]
    #[case("[number, string]", "number[]", FALSE)] // string not <: number
    #[case("[]", "number[]", TRUE)] // empty tuple to any array
    #[case("[]", "[]", TRUE)]
    #[case("[number]", "object", TRUE)]
    #[case("[number]", "{}", TRUE)]
    #[case("[number]", "Object", TRUE)]
    #[case("[number]", "string", FALSE)]
    // --- Non-empty TypeLiteral LHS ---
    #[case("{ x: 1 }", "{ x: number }", TRUE)]
    #[case("{ x: 1 }", "{ x: string }", FALSE)]
    #[case("{ x: 1 }", "{}", TRUE)]
    #[case("{ x: 1 }", "object", TRUE)]
    #[case("{ x: 1 }", "Object", TRUE)]
    #[case("{ x: 1 }", "{ x: 1, y: 2 }", FALSE)] // missing required property
    #[case("{ x: 1, y: 2 }", "{ x: number }", TRUE)] // width subtyping: extras allowed
    #[case("{ a: number }", "{ b: number }", FALSE)] // name mismatch
    #[case("{ x: number }", "{ x: 1 }", FALSE)] // base not assignable to literal
    #[case("{ x: { y: 1 } }", "{ x: { y: number } }", TRUE)] // depth
    #[case("{ x: { y: 1 } }", "{ x: { y: string } }", FALSE)]
    #[case("{ x: 1 }", "string", FALSE)]
    #[case("{ x: 1 }", "[number]", FALSE)]
    // optional target properties (newtype prefix `?` syntax)
    #[case("{ one: number }", "{ one: number, ?two: string }", TRUE)] // optional missing ok
    #[case("{ one: number, two: string }", "{ one: number, ?two: string }", TRUE)]
    #[case("{ one: number, two: number }", "{ one: number, ?two: string }", FALSE)] // present but wrong
    // optional source property may be absent → not assignable to a required target
    #[case("{ ?two: string }", "{ two: string }", FALSE)]
    #[case("{ ?two: string }", "{ ?two: string }", TRUE)] // optional → optional ok
    // optional target widens with undefined
    #[case("{ x: undefined }", "{ ?x: number }", TRUE)]
    // empty source is assignable to an all-optional target
    #[case("{}", "{ ?x: number }", TRUE)]
    #[case("{}", "{ ?a: string, ?b: number }", TRUE)]
    #[case("{}", "{ x: number }", FALSE)] // still false: required prop missing
    // --- Opaque / unresolvable references → indeterminate (Both) ---
    #[case("Foo", "Foo", TRUE)] // reflexive resolves first
    #[case("Foo", "any", TRUE)]
    #[case("Foo", "unknown", TRUE)]
    #[case("Foo", "never", FALSE)]
    #[case("Foo", "string", BOTH)]
    #[case("Foo", "object", BOTH)]
    #[case("Foo", "Object", BOTH)] // wrapper target does not preempt the free variable
    #[case("Foo", "{ x: 1 }", BOTH)]
    #[case("A::B", "A::B", TRUE)]
    #[case("A::B", "string", BOTH)]
    #[case("A[B]", "string", BOTH)] // indexed access
    #[case("A(B)", "string", BOTH)] // generic application
    #[case("keyof(A)", "string", BOTH)]
    // --- Union LHS / RHS ---
    #[case("1 | 2", "number", TRUE)] // every member <: number
    #[case("1 | 2", "1", FALSE)] // 2 is not <: 1
    #[case("'a' | 'b'", "string", TRUE)]
    #[case("1", "1 | 2", TRUE)] // assignable to some member
    #[case("3", "1 | 2", FALSE)]
    #[case("string", "string | number", TRUE)]
    #[case("string", "number | boolean", FALSE)]
    #[case("string | number", "number | string", TRUE)]
    #[case("1 | 2", "number | string", TRUE)]
    #[case("boolean", "true | false", TRUE)] // boolean is true | false
    // --- Intersection LHS / RHS ---
    #[case("{ one: number } & { two: string }", "{ one: number }", TRUE)] // some member
    #[case("{ one: number } & { two: string }", "{ two: string }", TRUE)]
    #[case("{ one: number } & { two: string }", "{ one: number, two: string }", TRUE)] // merged shape
    #[case("{ one: number } & { two: string }", "{ three: boolean }", FALSE)]
    #[case("{ a: 1 } & { b: 2 }", "{ a: number, b: number } | string", TRUE)] // merged vs union
    #[case("string", "string & Object", TRUE)] // assignable to every member
    #[case("string", "string & number", FALSE)]
    #[case("never & string", "number", NEVER)] // never & T == never
    #[trace]
    fn is_assignable_to_extended(
        #[case] a: &str,
        #[case] b: &str,
        #[case] expected: ExtendsResult,
    ) {
        assert_eq!(ast!(a).is_assignable_to(&ast!(b)), expected);
    }
}

mod simplify_tests {
    use newtype::parser::{self, Rule::expr};
    use pest::Parser;

    #[test]
    fn simplify_path_access() {
        let pairs = parser::NewtypeParser::parse(parser::Rule::expr, "A::B::C::D").unwrap();
        let actual = parser::parse_expr(pairs).simplify();
        insta::assert_snapshot!(actual.to_sexp().unwrap());
    }

    #[test]
    fn simplify_basic() {
        let actual = parse!(expr, "if a <: b then c else d end").simplify();
        insta::assert_snapshot!(actual.to_sexp().unwrap());
    }
}
