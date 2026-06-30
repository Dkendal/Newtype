use std::collections::{HashMap, HashSet};

use crate::{
    ast::{
        type_env::{fingerprint, substitute, ResolveCtx},
        Access, ApplyGeneric, Ast, Builtin, BuiltinKeyword, ExtendsExpr, FunctionType, Ident,
        IntersectionType, MappedType, MappingModifier, ObjectProperty, ObjectPropertyKey,
        PrimitiveType, Span, Tuple, TypeLiteral, TypeString, UnionType,
    },
    extends_result::ExtendsResult,
};

/// Structural classification of an object-property key, used to relate object
/// type literals that carry index signatures.
enum KeyClass<'a> {
    /// A plain named property, e.g. `a` in `{ a: T }`.
    Named(&'a str),
    /// A `string` index signature, `{ [k in string]: V }`.
    StringIndex,
    /// A `number` index signature, `{ [k in number]: V }`.
    NumberIndex,
    /// A key we cannot reason about structurally: a computed key, a remapped
    /// index, or an index over a non-`string`/`number` iterable.
    Other,
}

impl Ast {
    /// Assignability test mirroring the TypeScript checker's assignable
    /// relation (`internal/checker/relater.go`). Answers "is `self` assignable
    /// to `other`" in the sense of a conditional type `self extends other`.
    ///
    /// This is the purely-structural entry point: named references (idents,
    /// generic applications, …) are treated as free type variables. To resolve
    /// references against a program's definitions, use
    /// [`is_assignable_to_ctx`](Self::is_assignable_to_ctx) with a populated
    /// [`ResolveCtx`].
    pub fn is_assignable_to(&self, other: &Ast) -> ExtendsResult {
        self.is_assignable_to_ctx(other, &ResolveCtx::empty())
    }

    /// Assignability under a resolution context. When `ctx` carries an
    /// environment, named references on either side are resolved to their
    /// definitions and the check recurses; a coinductive assumption set keeps
    /// recursive definitions terminating.
    ///
    /// The four outcomes are interpreted by the runtime (`runtime::builtin::unquote`):
    /// `True` → take the `then` branch; `False` → take the `else` branch;
    /// `Never` → the LHS is the bottom type, collapse to `never`; `Both` → the
    /// relation is indeterminate (the LHS is `any`, or an unresolvable
    /// reference), so the union of both branches is produced.
    pub fn is_assignable_to_ctx(&self, other: &Ast, ctx: &ResolveCtx) -> ExtendsResult {
        type A = Ast;
        type T = ExtendsResult;

        // Resolve named references against the environment first. If either side
        // names a definition, expand it and recurse — guarding the relation so a
        // recursive definition encountered while proving itself is taken to hold.
        if let Some(env) = ctx.env() {
            let lhs_resolved = env.resolve_head(self);
            let rhs_resolved = env.resolve_head(other);

            if lhs_resolved.is_some() || rhs_resolved.is_some() {
                let pair = (fingerprint(self), fingerprint(other));
                if !ctx.assume(&pair) {
                    return T::True;
                }
                let lhs = lhs_resolved.as_ref().unwrap_or(self);
                let rhs = rhs_resolved.as_ref().unwrap_or(other);
                let result = lhs.is_assignable_to_ctx(rhs, ctx);
                ctx.discharge(&pair);
                return result;
            }
        }

        // Conditional types (`check extends T ? then : else`) on either side are
        // reduced to the selected branch before relating. Distribution over a
        // naked union check type is handled upstream (at instantiation, in
        // `TypeEnv`), so by this point the check type is a single type and a
        // plain reduction — plus `infer` extraction — is correct.
        if let Ast::ExtendsExpr(e) = self {
            let reduced = Self::reduce_conditional(e, ctx);
            return reduced.is_assignable_to_ctx(other, ctx);
        }
        if let Ast::ExtendsExpr(e) = other {
            let reduced = Self::reduce_conditional(e, ctx);
            return self.is_assignable_to_ctx(&reduced, ctx);
        }

        // Literal/primitive source assignable to its own bare primitive target
        // (e.g. `1 <: number`, `"x" <: string`, `{} <: object`). Object wrappers
        // are excluded: a wrapper (`String`, `Number`, …) is an object value and
        // is NOT assignable to its bare primitive, so it must not collapse here.
        if let Ast::Primitive(other, _) = other {
            if !self.is_object_wrapper() {
                if let Some(value) = self.get_primitive_type() {
                    if value == *other {
                        return T::True;
                    }
                }
            }
        }

        match (self, other) {
            (x, _) if !x.is_typescript_feature() => {
                unreachable!()
            }

            (A::NeverKeyword(_), _) => T::Never,

            (lhs, rhs) if lhs == rhs => T::True,

            (_, rhs) if rhs.is_top_type() => T::True,

            (A::AnyKeyword(_), _) => T::Both,

            (A::UnknownKeyword(_), _) => T::False,

            // `keyof X` on either side. When `X` is (or resolves to) an object
            // literal, evaluate `keyof X` to the union of its plain string-named
            // keys and relate via the normal union logic. Both arms precede the
            // set-operation arms below: a source `keyof` must expand before the
            // target-union arm would otherwise distribute over it, and a target
            // `keyof` must expand before the structural arms. A non-enumerable
            // argument (arrays, primitives, unresolved refs, index signatures,
            // mapped types) leaves the relation indeterminate.
            (
                A::Builtin(Builtin {
                    name: BuiltinKeyword::Keyof,
                    argument,
                    ..
                }),
                rhs,
            ) => match Self::eval_keyof(argument, ctx) {
                Some(keys) => keys.is_assignable_to_ctx(rhs, ctx),
                None => T::Both,
            },

            (
                lhs,
                A::Builtin(Builtin {
                    name: BuiltinKeyword::Keyof,
                    argument,
                    ..
                }),
            ) => match Self::eval_keyof(argument, ctx) {
                Some(keys) => lhs.is_assignable_to_ctx(&keys, ctx),
                None => T::Both,
            },

            // Mapped types `{ [K in T]: V }`. When the key set `T` is a
            // statically-known set of literal keys (a union/singleton of string
            // literals, `never`, or `keyof O` for an object literal `O`), expand
            // the mapped type to the equivalent object literal and relate
            // structurally. An unknown key set (a `string`/`number` primitive
            // iterable, a template remap, an unresolved reference) leaves the
            // relation indeterminate. The target arm must precede the structural
            // `TypeLiteral`/`Tuple`/… source arms below, which would otherwise
            // mishandle a mapped-type target.
            (A::MappedType(mt), rhs) => match Self::expand_mapped_type(mt, ctx) {
                Some(tl) => Ast::TypeLiteral(tl).is_assignable_to_ctx(rhs, ctx),
                None => T::Both,
            },

            (lhs, A::MappedType(mt)) => match Self::expand_mapped_type(mt, ctx) {
                Some(tl) => lhs.is_assignable_to_ctx(&Ast::TypeLiteral(tl), ctx),
                None => T::Both,
            },

            // Set operations. Source-union: every member must be assignable.
            // Source-intersection: some member (or the merged shape) must be.
            // Target-union: assignable to some member. Target-intersection:
            // assignable to every member. Source arms come first so that
            // `(A | B) <: (C | D)` distributes correctly, and — crucially — so a
            // source intersection that reduces to `never` (e.g. `string & number`)
            // collapses to `Never` even against a `never` target, instead of being
            // caught by the `(_, never) => False` arm below.
            (A::UnionType(UnionType { types, .. }), rhs) => {
                Self::union_source_assignable(types, rhs, ctx)
            }

            (A::IntersectionType(IntersectionType { types, .. }), rhs) => {
                Self::intersection_source_assignable(types, rhs, ctx)
            }

            (_, A::NeverKeyword(_)) => T::False,

            (lhs, A::UnionType(UnionType { types, .. })) => {
                Self::assignable_to_union(lhs, types, ctx)
            }

            (lhs, A::IntersectionType(IntersectionType { types, .. })) => {
                Self::assignable_to_intersection(lhs, types, ctx)
            }

            // `readonly` arrays/tuples. The relation is one-directional: a
            // mutable array/tuple is assignable to a `readonly` one, but not
            // the reverse. A readonly source is thus unassignable to a mutable
            // array/tuple target; against any other target it behaves like the
            // underlying array/tuple. These arms must precede the source-side
            // `Array`/`Tuple` arms so a `readonly` operand is not mishandled.
            (A::Readonly(src), rhs) => match rhs {
                A::Readonly(tgt) => src.is_assignable_to_ctx(tgt, ctx),
                A::Array(_) | A::Tuple(_) => T::False,
                _ => src.is_assignable_to_ctx(rhs, ctx),
            },

            (lhs, A::Readonly(tgt)) => lhs.is_assignable_to_ctx(tgt, ctx),

            // Indexed-access / generic-application / qualified references cannot
            // be resolved without a type environment (bindings are substituted
            // away upstream), so a reference reaching this engine is a free type
            // variable: indeterminate.
            (A::Access(Access { .. }), _) => T::Both,

            (A::ApplyGeneric(_), _) => T::Both,

            (A::Array(element), rhs) => Self::array_assignable_to(element, rhs, ctx),

            (A::Path(_), _) => T::Both,

            // A bare, unresolvable type reference (e.g. `Foo`) is a free type
            // variable — indeterminate, like the deferred conditional TypeScript
            // would produce. Object-wrapper idents (`String`, `Object`, …) have a
            // primitive type and are handled structurally further down, so they
            // are excluded here. This must precede the `Object`-wrapper target
            // arm below, otherwise `Foo <: Object` would wrongly resolve to True.
            // The global `Function` interface is an object value that carries
            // call signatures: it is assignable to the any-object targets
            // (`object`, `{}`, `Object`) but NOT to any specific function type
            // (its signature is `(...args: any[]) => any`, which is not
            // assignable to an arbitrary concrete signature). Must precede the
            // free-variable `Ident` arm below.
            (lhs, rhs) if lhs.is_function_interface() => {
                if Self::accepts_any_object(rhs) {
                    T::True
                } else {
                    T::False
                }
            }

            (lhs @ A::Ident(_), _) if lhs.get_primitive_type().is_none() => T::Both,

            (lhs, rhs) if lhs.is_non_nullish() && rhs.is_object_object_wrapper() => T::True,

            (A::TypeLiteral(lhs), rhs) => Self::type_literal_assignable_to(lhs, rhs, ctx),

            // `undefined` is assignable to `void` under --strict (the `void`
            // domain includes `undefined`). The relation is one-directional:
            // `void <: undefined` is false, and `null` is assignable to
            // neither — those fall through to the identity check below.
            (A::Primitive(PrimitiveType::Undefined, _), A::Primitive(PrimitiveType::Void, _)) => {
                T::True
            }

            (A::Primitive(lhs, _), A::Primitive(rhs, _)) => Into::into(lhs == rhs),

            (A::TemplateString(_) | A::TypeString(_), A::Primitive(PrimitiveType::String, _)) => {
                T::True
            }

            (
                A::Primitive(PrimitiveType::String, _) | A::TemplateString(_) | A::TypeString(_),
                rhs,
            ) if rhs.is_string_object_wrapper() => T::True,

            (A::TypeNumber(_), A::Primitive(PrimitiveType::Number, _)) => T::True,

            // Object wrappers as TARGETS (`Number`, `Boolean`, `BigInt`,
            // `Symbol` — `Object` is the any-object target handled above). A
            // value is assignable to the wrapper iff its primitive type matches
            // the wrapper's primitive: this admits the bare primitive
            // (`number <: Number`), the matching literals (`1 <: Number`,
            // `true <: Boolean`), and the same wrapper (`Number <: Number`),
            // while rejecting cross-primitive and non-primitive sources.
            (lhs, rhs) if rhs.is_object_wrapper() && !rhs.is_object_object_wrapper() => {
                // `get_primitive_type` maps literals and wrapper idents to their
                // primitive but not a bare `Primitive`, so match that directly.
                let lhs_prim = match lhs {
                    A::Primitive(p, _) => Some(p.clone()),
                    _ => lhs.get_primitive_type(),
                };
                match (lhs_prim, rhs.get_primitive_type()) {
                    (Some(l), Some(r)) if l == r => T::True,
                    _ => T::False,
                }
            }

            // Object wrappers as SOURCES are object values: assignable to the
            // any-object targets (`{}`, `object`, `Object` — `Object` is handled
            // by the non-nullish arm above), but NOT to their bare primitive.
            (lhs, rhs) if lhs.is_object_wrapper() => Into::into(Self::accepts_any_object(rhs)),

            (A::Tuple(tuple), rhs) => Self::tuple_assignable_to(tuple, rhs, ctx),

            (A::FunctionType(lhs), rhs) => Self::function_assignable_to(lhs, rhs, ctx),

            (_, _) => T::False,
        }
    }

    /// `[A, B, …]` (array element type `element`) assignable to `rhs`.
    fn array_assignable_to(element: &Ast, rhs: &Ast, ctx: &ResolveCtx) -> ExtendsResult {
        match rhs {
            // Covariant element relation: `A[] <: B[]` iff `A <: B`.
            Ast::Array(rhs_element) => {
                ExtendsResult::True.and(element.is_assignable_to_ctx(rhs_element, ctx))
            }
            // A variable-length array is not assignable to a fixed-arity tuple.
            Ast::Tuple(_) => ExtendsResult::False,
            rhs if Self::accepts_any_object(rhs) => ExtendsResult::True,
            _ => ExtendsResult::False,
        }
    }

    /// `(p…) => R` assignable to `rhs`. A function value is also an object, so
    /// it is assignable to the global `Function` interface and the any-object
    /// targets. Against another function type the relation mirrors the
    /// TypeScript checker's `compareSignatures`
    /// (`internal/checker/relater.go`): the source may omit trailing
    /// parameters but must not require more than the target supplies,
    /// parameters relate contravariantly (strict function types), and the
    /// return type relates covariantly — except a target return of `any` or
    /// `void` accepts any source return.
    fn function_assignable_to(lhs: &FunctionType, rhs: &Ast, ctx: &ResolveCtx) -> ExtendsResult {
        let Ast::FunctionType(target) = rhs else {
            return if rhs.is_function_interface() || Self::accepts_any_object(rhs) {
                ExtendsResult::True
            } else {
                ExtendsResult::False
            };
        };

        // Normalize each signature into its fixed (leading, non-rest) parameter
        // types plus an optional rest element type — a `...a: T[]` rest absorbs
        // any number of trailing arguments of type `T`.
        let (source_fixed, source_rest) = Self::split_params(&lhs.params);
        let (target_fixed, target_rest) = Self::split_params(&target.params);

        // Arity. The source must not require more parameters than the target can
        // supply: a target rest parameter supplies unboundedly, otherwise only
        // its fixed parameters. Fewer source parameters are fine (the source
        // simply ignores the extra arguments the target passes).
        let source_required = source_fixed.len();
        let target_supply = if target_rest.is_some() {
            usize::MAX
        } else {
            target_fixed.len()
        };
        if source_required > target_supply {
            return ExtendsResult::False;
        }

        // Parameters are contravariant: at each position the target supplies, the
        // target parameter type must be assignable to the source parameter type.
        // An `any` in either position is bidirectionally compatible. A position
        // the source does not cover (no fixed parameter and no rest) imposes no
        // constraint — the source ignores that argument.
        let mut acc = ExtendsResult::True;
        let positions = source_fixed.len().max(target_fixed.len());
        for i in 0..positions {
            let source_ty = source_fixed.get(i).copied().or(source_rest);
            let target_ty = target_fixed.get(i).copied().or(target_rest);
            let (Some(source_ty), Some(target_ty)) = (source_ty, target_ty) else {
                continue;
            };
            let related = if matches!(source_ty, Ast::AnyKeyword(_))
                || matches!(target_ty, Ast::AnyKeyword(_))
            {
                ExtendsResult::True
            } else {
                target_ty.is_assignable_to_ctx(source_ty, ctx)
            };
            acc = acc.and(related);
        }

        // Return type. A target return of `any` or `void` is satisfied by any
        // source return; otherwise the relation is covariant. A source return of
        // `any` is assignable to every target return except `never` (`any` is not
        // assignable to the bottom type).
        let target_return = target.return_type.as_ref();
        let target_return_absorbs = matches!(
            target_return,
            Ast::AnyKeyword(_) | Ast::Primitive(PrimitiveType::Void, _)
        );
        let return_relation = if target_return_absorbs {
            ExtendsResult::True
        } else if matches!(lhs.return_type.as_ref(), Ast::AnyKeyword(_)) {
            Into::into(!matches!(target_return, Ast::NeverKeyword(_)))
        } else {
            lhs.return_type.is_assignable_to_ctx(target_return, ctx)
        };

        acc.and(return_relation)
    }

    /// Split a parameter list into its leading fixed (non-rest) parameter types
    /// and the element type of a trailing rest parameter, if any. A rest
    /// parameter `...a: T[]` contributes element type `T`; a non-array rest type
    /// contributes itself.
    fn split_params(params: &[crate::ast::Parameter]) -> (Vec<&Ast>, Option<&Ast>) {
        let mut fixed = Vec::new();
        let mut rest = None;
        for param in params {
            if param.ellipsis {
                rest = Some(match &param.kind {
                    Ast::Array(element) => element.as_ref(),
                    other => other,
                });
                break;
            }
            fixed.push(&param.kind);
        }
        (fixed, rest)
    }

    /// `[A, B, …]` tuple assignable to `rhs`.
    fn tuple_assignable_to(tuple: &Tuple, rhs: &Ast, ctx: &ResolveCtx) -> ExtendsResult {
        match rhs {
            // Element-wise, same arity (no optional/rest modelling in tuples).
            Ast::Tuple(other) => {
                if tuple.items.len() != other.items.len() {
                    return ExtendsResult::False;
                }
                tuple
                    .items
                    .iter()
                    .zip(other.items.iter())
                    .fold(ExtendsResult::True, |acc, (a, b)| {
                        acc.and(a.is_assignable_to_ctx(b, ctx))
                    })
            }
            // `[A, B] <: T[]` iff every element is assignable to `T`. An empty
            // tuple is vacuously assignable to any array.
            Ast::Array(rhs_element) => tuple.items.iter().fold(ExtendsResult::True, |acc, item| {
                acc.and(item.is_assignable_to_ctx(rhs_element, ctx))
            }),
            rhs if Self::accepts_any_object(rhs) => ExtendsResult::True,
            _ => ExtendsResult::False,
        }
    }

    /// Object type literal assignable to `rhs`. Structural width and depth
    /// subtyping: every required property of the target must be present on the
    /// source with an assignable value; extra source properties are allowed (no
    /// fresh-literal excess-property check at this layer). Handles the empty
    /// source `{}` too — it is assignable to any all-optional target.
    fn type_literal_assignable_to(lhs: &TypeLiteral, rhs: &Ast, ctx: &ResolveCtx) -> ExtendsResult {
        match rhs {
            rhs if Self::accepts_any_object(rhs) => ExtendsResult::True,
            Ast::TypeLiteral(target) => {
                // Any key we can't reason about structurally — a computed key, a
                // remapped index, or an index whose iterable is neither the
                // `string` nor the `number` primitive (a union/keyof/mapped
                // iterable) — leaves the whole relation indeterminate.
                let any_other = lhs
                    .iter()
                    .chain(target.iter())
                    .any(|p| matches!(Self::classify_key(&p.key), KeyClass::Other));
                if any_other {
                    return ExtendsResult::Both;
                }

                // Index the source by kind: named properties (by name), and the
                // optional string/number index-signature value types. A source
                // index signature does NOT supply named properties.
                let mut source_named: Vec<(&str, &ObjectProperty)> = vec![];
                let mut source_string_index: Option<&Ast> = None;
                let mut source_number_index: Option<&Ast> = None;
                for p in lhs.iter() {
                    match Self::classify_key(&p.key) {
                        KeyClass::Named(name) => source_named.push((name, p)),
                        KeyClass::StringIndex => source_string_index = Some(&p.value),
                        KeyClass::NumberIndex => source_number_index = Some(&p.value),
                        KeyClass::Other => unreachable!("filtered out above"),
                    }
                }

                let mut acc = ExtendsResult::True;
                for target_prop in target.iter() {
                    let relation = match Self::classify_key(&target_prop.key) {
                        KeyClass::Named(name) => {
                            match source_named.iter().find(|(n, _)| *n == name) {
                                Some((_, source_prop)) => {
                                    Self::property_relation(source_prop, target_prop, ctx)
                                }
                                // A source index signature does NOT satisfy a
                                // required named target.
                                None if target_prop.optional => ExtendsResult::True,
                                None => ExtendsResult::False,
                            }
                        }
                        // A target string index must accept every source member
                        // (every named value, plus either index value type).
                        KeyClass::StringIndex => {
                            let vt = &target_prop.value;
                            let mut r = ExtendsResult::True;
                            for (_, source_prop) in &source_named {
                                r = r.and(source_prop.value.is_assignable_to_ctx(vt, ctx));
                            }
                            if let Some(sv) = source_string_index {
                                r = r.and(sv.is_assignable_to_ctx(vt, ctx));
                            }
                            if let Some(sv) = source_number_index {
                                r = r.and(sv.is_assignable_to_ctx(vt, ctx));
                            }
                            r
                        }
                        // A target number index constrains only numeric-named
                        // source members, plus either index value type.
                        KeyClass::NumberIndex => {
                            let vt = &target_prop.value;
                            let mut r = ExtendsResult::True;
                            for (name, source_prop) in &source_named {
                                if Self::is_numeric_name(name) {
                                    r = r.and(source_prop.value.is_assignable_to_ctx(vt, ctx));
                                }
                            }
                            if let Some(sv) = source_number_index {
                                r = r.and(sv.is_assignable_to_ctx(vt, ctx));
                            }
                            if let Some(sv) = source_string_index {
                                r = r.and(sv.is_assignable_to_ctx(vt, ctx));
                            }
                            r
                        }
                        KeyClass::Other => unreachable!("filtered out above"),
                    };
                    acc = acc.and(relation);
                }
                acc
            }
            _ => ExtendsResult::False,
        }
    }

    /// Classify an object-property key for the structural index-signature
    /// relation. A plain key is `Named`; a `[k in string]` / `[k in number]`
    /// index signature is `StringIndex` / `NumberIndex`; everything else (a
    /// computed key, a remapped index, or an index over a non-`string`/`number`
    /// iterable such as a union, `keyof`, or mapped iterable) is `Other`.
    fn classify_key(key: &ObjectPropertyKey) -> KeyClass<'_> {
        match key {
            ObjectPropertyKey::Key(name) => KeyClass::Named(name),
            ObjectPropertyKey::Index(index) if index.remapped_as.is_none() => match &index.iterable
            {
                Ast::Primitive(PrimitiveType::String, _) => KeyClass::StringIndex,
                Ast::Primitive(PrimitiveType::Number, _) => KeyClass::NumberIndex,
                _ => KeyClass::Other,
            },
            _ => KeyClass::Other,
        }
    }

    /// Whether a plain property name is a numeric key (constrained by a `number`
    /// index signature). TypeScript treats a property whose name is a valid
    /// number literal as a numeric key.
    fn is_numeric_name(name: &str) -> bool {
        name.parse::<f64>().is_ok()
    }

    /// Relation between a present source property and the target property it
    /// matches by name.
    fn property_relation(
        source: &ObjectProperty,
        target: &ObjectProperty,
        ctx: &ResolveCtx,
    ) -> ExtendsResult {
        // An optional source property may be absent, so it is not assignable to a
        // required target property.
        if source.optional && !target.optional {
            return ExtendsResult::False;
        }

        let mut value = source.value.is_assignable_to_ctx(&target.value, ctx);

        // An optional target property has type `T | undefined`, so an
        // `undefined`-typed source value still satisfies it.
        if target.optional {
            let undefined = Ast::Primitive(PrimitiveType::Undefined, source.value.as_span());
            value = value.or(source.value.is_assignable_to_ctx(&undefined, ctx));
        }

        value
    }

    /// Source union: assignable iff *every* member is assignable to `rhs`.
    fn union_source_assignable(members: &[Ast], rhs: &Ast, ctx: &ResolveCtx) -> ExtendsResult {
        members.iter().fold(ExtendsResult::True, |acc, member| {
            acc.and(member.is_assignable_to_ctx(rhs, ctx))
        })
    }

    /// Target union: assignable iff `lhs` is assignable to *some* member.
    fn assignable_to_union(lhs: &Ast, members: &[Ast], ctx: &ResolveCtx) -> ExtendsResult {
        // `boolean` is the union `true | false`; it is assignable to a target
        // union only if both of its members are.
        if matches!(lhs, Ast::Primitive(PrimitiveType::Boolean, _)) {
            let span = lhs.as_span();
            let t = Ast::TrueKeyword(span);
            let f = Ast::FalseKeyword(span);
            return Self::assignable_to_union(&t, members, ctx)
                .and(Self::assignable_to_union(&f, members, ctx));
        }

        members.iter().fold(ExtendsResult::False, |acc, member| {
            acc.or(lhs.is_assignable_to_ctx(member, ctx))
        })
    }

    /// Target intersection: assignable iff `lhs` is assignable to *every* member.
    fn assignable_to_intersection(lhs: &Ast, members: &[Ast], ctx: &ResolveCtx) -> ExtendsResult {
        members.iter().fold(ExtendsResult::True, |acc, member| {
            acc.and(lhs.is_assignable_to_ctx(member, ctx))
        })
    }

    /// Source intersection: assignable iff *some* member is assignable, or —
    /// for an object target — the merged shape of all object members is.
    fn intersection_source_assignable(
        members: &[Ast],
        rhs: &Ast,
        ctx: &ResolveCtx,
    ) -> ExtendsResult {
        // `never & T` reduces to `never`, which collapses the conditional.
        if members.iter().any(|m| matches!(m, Ast::NeverKeyword(_))) {
            return ExtendsResult::Never;
        }

        // A genuinely contradictory intersection is uninhabited, so it reduces
        // to `never` (the bottom type), mirroring TS. Two members are
        // contradictory when both are drawn from disjoint *unit* primitive/literal
        // kinds (`string`/`number`/`boolean`/`bigint`/`symbol` and their literals)
        // yet share no common subtype — neither is assignable to the other. This
        // catches `string & number`, `string & boolean`, `1 & 2`, `true & false`,
        // while leaving `1 & number` (→ `1`) and `string & "a"` (→ `"a"`) alone,
        // since there one member is assignable to the other. Object members
        // (`{a:1} & {b:2}`, `string & {}`) never participate here — disjoint object
        // shapes intersect to a non-`never` type and are handled by the merge below.
        for (i, a) in members.iter().enumerate() {
            if Self::disjoint_primitive_family(a).is_none() {
                continue;
            }
            for b in &members[i + 1..] {
                if Self::disjoint_primitive_family(b).is_none() {
                    continue;
                }
                let a_to_b = a.is_assignable_to_ctx(b, ctx);
                let b_to_a = b.is_assignable_to_ctx(a, ctx);
                if a_to_b != ExtendsResult::True && b_to_a != ExtendsResult::True {
                    return ExtendsResult::Never;
                }
            }
        }

        let some = members.iter().fold(ExtendsResult::False, |acc, member| {
            acc.or(member.is_assignable_to_ctx(rhs, ctx))
        });

        if some == ExtendsResult::True {
            return some;
        }

        // The intersection carries the union of all its object members'
        // properties, so merge them and try the combined shape against the
        // target (e.g. `{a:1} & {b:2} <: {a:number, b:number}`, and likewise
        // against a union target containing such a shape). The merged members
        // may themselves be references (e.g. an interface's `extends` parent),
        // so resolve each before merging.
        let resolved: Vec<Ast> = members
            .iter()
            .map(|member| Self::resolve_for_merge(member, ctx))
            .collect();

        if let Some(merged) = Self::merge_object_members(&resolved) {
            return some.or(Ast::TypeLiteral(merged).is_assignable_to_ctx(rhs, ctx));
        }

        some
    }

    /// Resolve a node to its definition body if it is a known reference, for the
    /// purpose of merging object members (interface inheritance). Non-references
    /// are returned unchanged.
    fn resolve_for_merge(member: &Ast, ctx: &ResolveCtx) -> Ast {
        ctx.env()
            .and_then(|env| env.resolve_head(member))
            .unwrap_or_else(|| member.clone())
    }

    /// Collect the properties of every object-literal member into one shape.
    fn merge_object_members(members: &[Ast]) -> Option<TypeLiteral> {
        let mut properties = vec![];
        let mut span = None;

        for member in members {
            if let Ast::TypeLiteral(tl) = member {
                if span.is_none() {
                    span = Some(tl.span);
                }
                properties.extend(tl.properties.clone());
            }
        }

        span.map(|span| TypeLiteral { properties, span })
    }

    /// Targets that accept any object value: the empty object `{}`, the
    /// `object` primitive, and the `Object` interface.
    fn accepts_any_object(rhs: &Ast) -> bool {
        rhs.is_empty_object()
            || matches!(rhs, Ast::Primitive(PrimitiveType::Object, _))
            || rhs.is_object_interface()
    }

    /// The "unit" primitive family of an intersection member, used to detect a
    /// contradictory (uninhabited) intersection. Returns the family for a bare
    /// primitive keyword or a literal of `string`/`number`/`boolean`/`bigint`/
    /// `symbol`. Object-ish kinds (object literals, the `object` primitive, the
    /// wrapper objects like `String`/`Number`) and the nullish primitives
    /// (`void`/`undefined`/`null`) are excluded — their intersections are not
    /// contradictory in this disjoint-primitive sense.
    fn disjoint_primitive_family(ast: &Ast) -> Option<PrimitiveType> {
        // Object-wrapper idents (`String`, `Number`, …) are object values, not the
        // bare primitive, so they do not participate in primitive contradiction.
        if ast.is_object_wrapper() {
            return None;
        }

        // `get_primitive_type` maps literals/wrappers to their primitive but not a
        // bare `Primitive`, so match that directly.
        let prim = match ast {
            Ast::Primitive(p, _) => p.clone(),
            _ => ast.get_primitive_type()?,
        };

        match prim {
            PrimitiveType::String
            | PrimitiveType::Number
            | PrimitiveType::Boolean
            | PrimitiveType::BigInt
            | PrimitiveType::Symbol => Some(prim),
            PrimitiveType::Object
            | PrimitiveType::Void
            | PrimitiveType::Undefined
            | PrimitiveType::Null => None,
        }
    }

    /// Evaluate `keyof arg` to the set of its plain string-named keys, as
    /// `TypeString` literal nodes. Returns `None` (indeterminate) unless `arg`
    /// is — or resolves via the environment to — an object literal whose keys
    /// are *all* plain names (`ObjectPropertyKey::Key`). Index, computed, or
    /// remapped keys, and non-object arguments, are not enumerable here and
    /// yield `None`. Exposed as a reusable building block (mapped types).
    fn keyof_string_keys(arg: &Ast, ctx: &ResolveCtx) -> Option<Vec<Ast>> {
        let resolved = ctx.env().and_then(|env| env.resolve_head(arg));
        let target = resolved.as_ref().unwrap_or(arg);

        let Ast::TypeLiteral(literal) = target else {
            return None;
        };

        let mut keys = Vec::with_capacity(literal.properties.len());
        for prop in literal.iter() {
            match &prop.key {
                ObjectPropertyKey::Key(name) => keys.push(Ast::TypeString(TypeString {
                    ty: name.clone(),
                    span: literal.span,
                })),
                // A non-plain key can't be enumerated structurally.
                _ => return None,
            }
        }
        Some(keys)
    }

    /// Evaluate `keyof arg` to a single relatable type: the union of its
    /// string-named keys (a bare key when there is one, `never` for the empty
    /// object). `None` when the argument is not an enumerable object literal.
    fn eval_keyof(arg: &Ast, ctx: &ResolveCtx) -> Option<Ast> {
        let keys = Self::keyof_string_keys(arg, ctx)?;
        let span = arg.as_span();
        Some(match keys.len() {
            0 => Ast::NeverKeyword(span),
            1 => keys.into_iter().next().unwrap(),
            _ => Ast::UnionType(UnionType { types: keys, span }),
        })
    }

    /// Expand a mapped type `{ [K in T]: V }` to the equivalent object literal,
    /// when its key set `T` is a statically-known set of literal string keys.
    /// Returns `None` (so the caller stays indeterminate) when the key set isn't
    /// enumerable, or when the mapped type carries an `as` key-remap clause
    /// (deferred). For each key, the index variable `K` is substituted in the
    /// body `V`, and a homomorphic indexed-access body `O[K]` is reduced one step
    /// when it resolves; an irreducible body is left in place (relating as `Both`
    /// later). The mapped type's `+optional`/`+readonly` modifiers carry onto the
    /// generated properties.
    fn expand_mapped_type(mt: &MappedType, ctx: &ResolveCtx) -> Option<TypeLiteral> {
        // An `as` remap clause rewrites keys; defer that for now.
        if mt.remapped_as.is_some() {
            return None;
        }

        let keys = Self::mapped_key_set(&mt.iterable, ctx, &mut HashSet::new())?;

        let optional = matches!(mt.optional_mod, Some(MappingModifier::Add));
        let readonly = matches!(mt.readonly_mod, Some(MappingModifier::Add));

        let mut properties = Vec::with_capacity(keys.len());
        for key in keys {
            // The key must be a plain string literal to name a property.
            let Ast::TypeString(TypeString { ty: name, .. }) = &key else {
                return None;
            };

            // Substitute the index variable with this key in the body, then try a
            // one-step reduction of a homomorphic `O[K]` indexed access.
            let mut bindings = HashMap::new();
            bindings.insert(mt.index.clone(), key.clone());
            let body = substitute(&mt.body, &bindings);
            let value = Self::reduce_indexed_access(&body, ctx);

            properties.push(ObjectProperty {
                readonly,
                optional,
                key: ObjectPropertyKey::Key(name.clone()),
                value,
                span: mt.span,
            });
        }

        Some(TypeLiteral {
            properties,
            span: mt.span,
        })
    }

    /// Resolve the key set of a mapped type's iterable to a vector of literal key
    /// nodes, or `None` when it isn't a statically-known set of string-literal
    /// keys. A `never` iterable yields the empty set (`{}`); a single string
    /// literal a singleton; a union of string literals those keys; `keyof O` the
    /// keys of an object literal `O`; a reference is resolved one step and
    /// recursed (guarded by `seen` so a cyclic alias terminates).
    fn mapped_key_set(
        iterable: &Ast,
        ctx: &ResolveCtx,
        seen: &mut HashSet<String>,
    ) -> Option<Vec<Ast>> {
        match iterable {
            Ast::NeverKeyword(_) => Some(vec![]),
            Ast::TypeString(_) => Some(vec![iterable.clone()]),
            Ast::UnionType(UnionType { types, .. }) => {
                let mut keys = Vec::with_capacity(types.len());
                for ty in types {
                    match ty {
                        Ast::TypeString(_) => keys.push(ty.clone()),
                        _ => return None,
                    }
                }
                Some(keys)
            }
            Ast::Builtin(Builtin {
                name: BuiltinKeyword::Keyof,
                argument,
                ..
            }) => Self::keyof_string_keys(argument, ctx),
            other => {
                if !seen.insert(fingerprint(other)) {
                    return None;
                }
                let resolved = ctx.env().and_then(|env| env.resolve_head(other))?;
                Self::mapped_key_set(&resolved, ctx, seen)
            }
        }
    }

    /// Reduce a conditional type `check extends extends_ty ? then : else` to the
    /// branch it selects. When `extends_ty` carries `infer` positions, the check
    /// type is structurally matched against the pattern to bind each `infer`
    /// variable; on a successful match the bindings are substituted into the
    /// `then` branch (duplicate covariant `infer` occurrences union), otherwise
    /// the `else` branch is taken. Without `infer`, the relation `check <:
    /// extends_ty` selects the branch (`True` → then, `False` → else, `Never` →
    /// `never`, indeterminate → the union of both branches).
    fn reduce_conditional(e: &ExtendsExpr, ctx: &ResolveCtx) -> Ast {
        let ExtendsExpr {
            lhs,
            rhs,
            then_branch,
            else_branch,
            span,
        } = e;

        // Resolve a named check type one step (e.g. an alias) before matching.
        let check = ctx
            .env()
            .and_then(|env| env.resolve_head(lhs))
            .unwrap_or_else(|| (**lhs).clone());

        if Self::contains_infer(rhs) {
            let mut bindings: HashMap<String, Vec<Ast>> = HashMap::new();
            if Self::match_infer(&check, rhs, ctx, &mut bindings) {
                let resolved: HashMap<String, Ast> = bindings
                    .into_iter()
                    .map(|(name, types)| (name, Self::union_of(types, *span)))
                    .collect();
                return substitute(then_branch, &resolved);
            }
            return (**else_branch).clone();
        }

        match check.is_assignable_to_ctx(rhs, ctx) {
            ExtendsResult::True => (**then_branch).clone(),
            ExtendsResult::False => (**else_branch).clone(),
            ExtendsResult::Never => Ast::NeverKeyword(*span),
            ExtendsResult::Both => Ast::UnionType(UnionType {
                types: vec![(**then_branch).clone(), (**else_branch).clone()],
                span: *span,
            }),
        }
    }

    /// Whether `ast` contains an `infer` node anywhere in its subtree.
    fn contains_infer(ast: &Ast) -> bool {
        let any = |items: &[Ast]| items.iter().any(Self::contains_infer);
        match ast {
            Ast::Infer(_) => true,
            Ast::Array(inner) | Ast::Readonly(inner) => Self::contains_infer(inner),
            Ast::ApplyGeneric(ApplyGeneric { receiver, args, .. }) => {
                Self::contains_infer(receiver) || any(args)
            }
            Ast::FunctionType(f) => {
                f.params.iter().any(|p| Self::contains_infer(&p.kind))
                    || Self::contains_infer(&f.return_type)
            }
            Ast::Tuple(t) => any(&t.items),
            Ast::TypeLiteral(l) => l.iter().any(|p| Self::contains_infer(&p.value)),
            Ast::UnionType(u) => any(&u.types),
            Ast::IntersectionType(i) => any(&i.types),
            Ast::Access(a) => Self::contains_infer(&a.lhs) || Self::contains_infer(&a.rhs),
            Ast::Builtin(b) => Self::contains_infer(&b.argument),
            _ => false,
        }
    }

    /// Combine inferred occurrences of a single `infer` variable into one type,
    /// deduplicating structurally and unioning multiple covariant occurrences.
    ///
    /// LIMITATION: TypeScript only unions duplicate `infer`s in *covariant*
    /// positions; in *contravariant* (parameter) positions it intersects them
    /// (so `(a: infer U, b: infer U) => any` against `(string, number) => any`
    /// yields `string & number`). We always union. This is currently latent —
    /// no caller reaches a contravariant duplicate, and the parser does not yet
    /// accept the multi-`infer` function syntax that would express it.
    fn union_of(types: Vec<Ast>, span: Span) -> Ast {
        let mut seen = HashSet::new();
        let mut unique = Vec::with_capacity(types.len());
        for ty in types {
            if seen.insert(fingerprint(&ty)) {
                unique.push(ty);
            }
        }
        match unique.len() {
            0 => Ast::NeverKeyword(span),
            1 => unique.into_iter().next().unwrap(),
            _ => Ast::UnionType(UnionType {
                types: unique,
                span,
            }),
        }
    }

    /// Structurally match a concrete `value` against an `infer`-bearing `pattern`,
    /// binding each `infer` variable to the corresponding part of `value`.
    /// Returns whether the shapes are compatible. `Array<T>`/`ReadonlyArray<T>`
    /// patterns match `T[]` values; generic applications match by receiver and
    /// arity; functions match parameter-wise and on the return type; tuples and
    /// object literals match structurally. A pattern subtree with no `infer`
    /// imposes the ordinary assignability constraint (`value <: pattern`).
    fn match_infer(
        value: &Ast,
        pattern: &Ast,
        ctx: &ResolveCtx,
        bindings: &mut HashMap<String, Vec<Ast>>,
    ) -> bool {
        match pattern {
            Ast::Infer(inner) => {
                if let Ast::Ident(Ident { name, .. }) = &**inner {
                    bindings
                        .entry(name.clone())
                        .or_default()
                        .push(value.clone());
                    true
                } else {
                    false
                }
            }

            Ast::Array(p_el) => match value {
                Ast::Array(v_el) => Self::match_infer(v_el, p_el, ctx, bindings),
                _ => false,
            },

            Ast::ApplyGeneric(ApplyGeneric { receiver, args, .. }) => {
                // `Array<X>` / `ReadonlyArray<X>` is the generic spelling of `X[]`.
                if let Ast::Ident(Ident { name, .. }) = &**receiver {
                    if (name == "Array" || name == "ReadonlyArray") && args.len() == 1 {
                        if let Ast::Array(v_el) = value {
                            return Self::match_infer(v_el, &args[0], ctx, bindings);
                        }
                    }
                }
                // Otherwise match against another application of the same generic.
                if let Ast::ApplyGeneric(ApplyGeneric {
                    receiver: v_recv,
                    args: v_args,
                    ..
                }) = value
                {
                    if fingerprint(receiver) == fingerprint(v_recv) && args.len() == v_args.len() {
                        return args
                            .iter()
                            .zip(v_args.iter())
                            .all(|(p, v)| Self::match_infer(v, p, ctx, bindings));
                    }
                }
                false
            }

            Ast::FunctionType(pattern_fn) => match value {
                Ast::FunctionType(value_fn) => {
                    if pattern_fn.params.len() != value_fn.params.len() {
                        return false;
                    }
                    for (pp, vp) in pattern_fn.params.iter().zip(value_fn.params.iter()) {
                        if !Self::match_infer(&vp.kind, &pp.kind, ctx, bindings) {
                            return false;
                        }
                    }
                    Self::match_infer(
                        &value_fn.return_type,
                        &pattern_fn.return_type,
                        ctx,
                        bindings,
                    )
                }
                _ => false,
            },

            Ast::Tuple(pattern_tuple) => match value {
                Ast::Tuple(value_tuple) => {
                    if pattern_tuple.items.len() != value_tuple.items.len() {
                        return false;
                    }
                    pattern_tuple
                        .items
                        .iter()
                        .zip(value_tuple.items.iter())
                        .all(|(p, v)| Self::match_infer(v, p, ctx, bindings))
                }
                _ => false,
            },

            Ast::TypeLiteral(pattern_lit) => match value {
                Ast::TypeLiteral(value_lit) => {
                    for pp in pattern_lit.iter() {
                        let ObjectPropertyKey::Key(key) = &pp.key else {
                            return false;
                        };
                        let matched = value_lit
                            .iter()
                            .find(|vp| matches!(&vp.key, ObjectPropertyKey::Key(vk) if vk == key));
                        match matched {
                            Some(vp) => {
                                if !Self::match_infer(&vp.value, &pp.value, ctx, bindings) {
                                    return false;
                                }
                            }
                            None => return false,
                        }
                    }
                    true
                }
                _ => false,
            },

            // A pattern subtree with no `infer` constrains the value structurally.
            _ => value.is_assignable_to_ctx(pattern, ctx) != ExtendsResult::False,
        }
    }

    /// One-step reduction of a homomorphic indexed-access body `O["k"]`: when `O`
    /// resolves to an object literal and `"k"` is one of its plain string keys,
    /// replace the access with that property's value. Any other body (a
    /// non-access, an unresolved object, a non-literal key, a missing key) is
    /// returned unchanged, to be related structurally (often `Both`) later.
    fn reduce_indexed_access(body: &Ast, ctx: &ResolveCtx) -> Ast {
        let Ast::Access(Access { lhs, rhs, .. }) = body else {
            return body.clone();
        };

        let Ast::TypeString(TypeString { ty: key_name, .. }) = rhs.as_ref() else {
            return body.clone();
        };

        let resolved = ctx
            .env()
            .and_then(|env| env.resolve_head(lhs))
            .unwrap_or_else(|| (**lhs).clone());
        let Ast::TypeLiteral(literal) = &resolved else {
            return body.clone();
        };

        for prop in literal.iter() {
            if let ObjectPropertyKey::Key(name) = &prop.key {
                if name == key_name {
                    return prop.value.clone();
                }
            }
        }

        body.clone()
    }
}
