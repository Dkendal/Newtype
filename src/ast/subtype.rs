use crate::{
    ast::{Access, Ast, Builtin, PrimitiveType},
    extends_result::ExtendsResult,
};

impl Ast {
    pub fn is_subtype(&self, other: &Ast) -> ExtendsResult {
        type A = Ast;
        type T = ExtendsResult;

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

            (A::Access(Access { .. }), _) => todo!(),

            (A::ApplyGeneric(_), _) => todo!(),

            (A::Array(_), _) => todo!(),

            (A::Builtin(Builtin { .. }), _) => todo!(),

            (A::Path(_), _) => todo!(),

            (lhs, rhs) if lhs.is_non_nullish() && rhs.is_object_object_wrapper() => T::True,

            (A::TypeLiteral(lhs), _) if lhs.is_empty() => match other {
                Ast::Primitive(PrimitiveType::Object, _) => T::True,
                ast if ast.is_object_interface() => T::True,
                _ => T::False,
            },

            (A::TypeLiteral(_), _) => todo!(),

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
                ast.is_subtype(rhs)
            }

            (A::Ident(_), _) => {
                todo!()
            }

            (A::Tuple(_), _) => todo!(),

            (a, b) => {
                dbg!(a, b);
                T::False
            }
        }
    }
}
