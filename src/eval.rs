use crate::ast::*;

pub fn eval(node: AstNode) {
    match &*node.value {
        Ast::Access { .. } => todo!(),
        Ast::Any => todo!(),
        Ast::Application(_) => todo!(),
        Ast::Array(_) => todo!(),
        Ast::InfixOp { .. } => todo!(),
        Ast::Builtin { .. } => todo!(),
        Ast::CondExpr(_) => todo!(),
        Ast::ExtendsInfixOp { .. } => todo!(),
        Ast::ExtendsExpr(_) => todo!(),
        Ast::ExtendsPrefixOp { .. } => todo!(),
        Ast::False => todo!(),
        Ast::Ident(_) => todo!(),
        Ast::IfExpr(_) => todo!(),
        Ast::ImportStatement { .. } => todo!(),
        Ast::LetExpr(_) => todo!(),
        Ast::MappedType(_) => todo!(),
        Ast::MatchExpr(_) => todo!(),
        Ast::NamespaceAccess(_) => todo!(),
        Ast::Never => todo!(),
        Ast::NoOp => todo!(),
        Ast::Null => todo!(),
        Ast::Number(_) => todo!(),
        Ast::ObjectLiteral(_) => todo!(),
        Ast::Primitive(_) => todo!(),
        Ast::Program(_) => todo!(),
        Ast::Statement(_) => todo!(),
        Ast::String(_) => todo!(),
        Ast::TemplateString(_) => todo!(),
        Ast::True => todo!(),
        Ast::Tuple(_) => todo!(),
        Ast::TypeAlias { .. } => todo!(),
        Ast::Undefined => todo!(),
        Ast::Unknown => todo!(),
        Ast::Interface(_) => todo!(),
        Ast::UnitTest(_) => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_newtype_program;

    #[ignore]
    #[test]
    fn test_eval() {
        let source = r#"
            type A as 1

            test "my test" do
                assert!(A == 1)
            end
        "#;

        let pairs = parse_newtype_program(source).unwrap();
        let node = pairs.simplify();

        eval(node);
    }
}
