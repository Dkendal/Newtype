use crate::{
    ast::{
        type_env::{fingerprint, ResolveCtx},
        Access, Ast, Builtin, IntersectionType, ObjectProperty, ObjectPropertyKey, PrimitiveType,
        Tuple, TypeLiteral, UnionType,
    },
    extends_result::ExtendsResult,
};

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

        if let Ast::Primitive(other, _) = other {
            if let Some(value) = self.get_primitive_type() {
                if value == *other {
                    return T::True;
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

            (_, A::NeverKeyword(_)) => T::False,

            // Set operations. Source-union: every member must be assignable.
            // Source-intersection: some member (or the merged shape) must be.
            // Target-union: assignable to some member. Target-intersection:
            // assignable to every member. Source arms come first so that
            // `(A | B) <: (C | D)` distributes correctly.
            (A::UnionType(UnionType { types, .. }), rhs) => {
                Self::union_source_assignable(types, rhs, ctx)
            }

            (A::IntersectionType(IntersectionType { types, .. }), rhs) => {
                Self::intersection_source_assignable(types, rhs, ctx)
            }

            (lhs, A::UnionType(UnionType { types, .. })) => {
                Self::assignable_to_union(lhs, types, ctx)
            }

            (lhs, A::IntersectionType(IntersectionType { types, .. })) => {
                Self::assignable_to_intersection(lhs, types, ctx)
            }

            // Indexed-access / generic-application / qualified references cannot
            // be resolved without a type environment (bindings are substituted
            // away upstream), so a reference reaching this engine is a free type
            // variable: indeterminate.
            (A::Access(Access { .. }), _) => T::Both,

            (A::ApplyGeneric(_), _) => T::Both,

            (A::Array(element), rhs) => Self::array_assignable_to(element, rhs, ctx),

            (A::Builtin(Builtin { .. }), _) => T::Both,

            (A::Path(_), _) => T::Both,

            // A bare, unresolvable type reference (e.g. `Foo`) is a free type
            // variable — indeterminate, like the deferred conditional TypeScript
            // would produce. Object-wrapper idents (`String`, `Object`, …) have a
            // primitive type and are handled structurally further down, so they
            // are excluded here. This must precede the `Object`-wrapper target
            // arm below, otherwise `Foo <: Object` would wrongly resolve to True.
            (lhs @ A::Ident(_), _) if lhs.get_primitive_type().is_none() => T::Both,

            (lhs, rhs) if lhs.is_non_nullish() && rhs.is_object_object_wrapper() => T::True,

            (A::TypeLiteral(lhs), rhs) => Self::type_literal_assignable_to(lhs, rhs, ctx),

            (A::Primitive(lhs, _), A::Primitive(rhs, _)) => Into::into(lhs == rhs),

            (A::TemplateString(_) | A::TypeString(_), A::Primitive(PrimitiveType::String, _)) => {
                T::True
            }

            (
                A::Primitive(PrimitiveType::String, _) | A::TemplateString(_) | A::TypeString(_),
                rhs,
            ) if rhs.is_string_object_wrapper() => T::True,

            (A::TypeNumber(_), A::Primitive(PrimitiveType::Number, _)) => T::True,

            // Object wrappers are equivalent to their primitive types in
            // this context.
            (lhs, rhs) if lhs.is_object_wrapper() => {
                let primitive_type = lhs.get_primitive_type().unwrap();
                let ast = Ast::Primitive(primitive_type, lhs.as_span());
                ast.is_assignable_to_ctx(rhs, ctx)
            }

            (A::Tuple(tuple), rhs) => Self::tuple_assignable_to(tuple, rhs, ctx),

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
            Ast::Array(rhs_element) => {
                tuple
                    .items
                    .iter()
                    .fold(ExtendsResult::True, |acc, item| {
                        acc.and(item.is_assignable_to_ctx(rhs_element, ctx))
                    })
            }
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
                // A source index/computed signature might cover a target key we
                // can't see structurally, so a "missing" required property is
                // indeterminate rather than a definite failure.
                let source_has_complex_key = lhs
                    .iter()
                    .any(|p| Self::property_key_name(&p.key).is_none());

                let mut acc = ExtendsResult::True;
                for target_prop in target.iter() {
                    // Index/computed target keys can't be compared structurally.
                    let Some(target_key) = Self::property_key_name(&target_prop.key) else {
                        return ExtendsResult::Both;
                    };

                    let source_prop = lhs.iter().find(|p| {
                        Self::property_key_name(&p.key)
                            .map(|k| k == target_key)
                            .unwrap_or(false)
                    });

                    match source_prop {
                        Some(source_prop) => {
                            acc = acc.and(Self::property_relation(source_prop, target_prop, ctx));
                        }
                        None if target_prop.optional => {}
                        None if source_has_complex_key => return ExtendsResult::Both,
                        None => return ExtendsResult::False,
                    }
                }
                acc
            }
            _ => ExtendsResult::False,
        }
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
        members
            .iter()
            .fold(ExtendsResult::True, |acc, member| {
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

        members
            .iter()
            .fold(ExtendsResult::False, |acc, member| {
                acc.or(lhs.is_assignable_to_ctx(member, ctx))
            })
    }

    /// Target intersection: assignable iff `lhs` is assignable to *every* member.
    fn assignable_to_intersection(lhs: &Ast, members: &[Ast], ctx: &ResolveCtx) -> ExtendsResult {
        members
            .iter()
            .fold(ExtendsResult::True, |acc, member| {
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

        let some = members
            .iter()
            .fold(ExtendsResult::False, |acc, member| {
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

    /// The static name of a plain property key, if it has one.
    fn property_key_name(key: &ObjectPropertyKey) -> Option<&str> {
        match key {
            ObjectPropertyKey::Key(name) => Some(name),
            _ => None,
        }
    }
}
