use crate::{
    ast::{
        type_env::{fingerprint, ResolveCtx},
        Access, Ast, Builtin, FunctionType, IntersectionType, ObjectProperty, ObjectPropertyKey,
        PrimitiveType, Tuple, TypeLiteral, UnionType,
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

            // `undefined` is assignable to `void` under --strict (the `void`
            // domain includes `undefined`). The relation is one-directional:
            // `void <: undefined` is false, and `null` is assignable to
            // neither — those fall through to the identity check below.
            (
                A::Primitive(PrimitiveType::Undefined, _),
                A::Primitive(PrimitiveType::Void, _),
            ) => T::True,

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

        // Arity. Parameters up to the first rest parameter are required; the
        // source must not require more than the target accepts (a trailing
        // rest parameter on the target absorbs any extra).
        let source_required = lhs.params.iter().take_while(|p| !p.ellipsis).count();
        let target_has_rest = target.params.last().is_some_and(|p| p.ellipsis);
        if !target_has_rest && source_required > target.params.len() {
            return ExtendsResult::False;
        }

        // Parameters are contravariant: each target parameter must be
        // assignable to the source parameter in the same position. An `any` in
        // either parameter position is bidirectionally compatible, so it
        // satisfies the relation outright (matching the TypeScript checker,
        // where `any` params are always related) rather than collapsing the
        // whole signature to the indeterminate `Both`.
        let mut acc = ExtendsResult::True;
        for (source_param, target_param) in lhs.params.iter().zip(target.params.iter()) {
            let related = if matches!(source_param.kind, Ast::AnyKeyword(_))
                || matches!(target_param.kind, Ast::AnyKeyword(_))
            {
                ExtendsResult::True
            } else {
                target_param.kind.is_assignable_to_ctx(&source_param.kind, ctx)
            };
            acc = acc.and(related);
        }

        // Return type. A target return of `any` or `void` is satisfied by any
        // source return; otherwise the relation is covariant.
        let target_return = target.return_type.as_ref();
        let target_return_absorbs = matches!(
            target_return,
            Ast::AnyKeyword(_) | Ast::Primitive(PrimitiveType::Void, _)
        );
        let return_relation = if target_return_absorbs {
            ExtendsResult::True
        } else {
            lhs.return_type.is_assignable_to_ctx(target_return, ctx)
        };

        acc.and(return_relation)
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

    /// The static name of a plain property key, if it has one.
    fn property_key_name(key: &ObjectPropertyKey) -> Option<&str> {
        match key {
            ObjectPropertyKey::Key(name) => Some(name),
            _ => None,
        }
    }
}
