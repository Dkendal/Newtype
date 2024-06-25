use pretty::RcDoc;

use crate::ast::*;

fn surround<'a, T>(doc: RcDoc<'a, ()>, left: T, right: T) -> RcDoc<()>
where
    T: Into<std::borrow::Cow<'a, str>>,
{
    RcDoc::text(left).append(doc).append(RcDoc::text(right))
}

fn parens(doc: RcDoc<()>) -> RcDoc<()> {
    surround(doc, "(", ")")
}

fn single_quote(doc: RcDoc<()>) -> RcDoc<()> {
    surround(doc, "'", "'")
}

fn double_quote(doc: RcDoc<()>) -> RcDoc<()> {
    surround(doc, "\"", "\"")
}

pub trait ToTypescript {
    fn to_pretty_ts(&self, width: usize) -> String {
        let mut w = Vec::new();
        self.to_ts().render(width, &mut w).unwrap();
        String::from_utf8(w).unwrap()
    }

    fn to_ts(&self) -> RcDoc<()>;
}

impl ToTypescript for Node {
    fn to_ts<'a>(&self) -> RcDoc<()> {
        match self {
            Node::Program(stmnts) => {
                let mut doc = RcDoc::nil();
                for stmnt in stmnts {
                    doc = doc.append(stmnt.to_ts()).append(RcDoc::hardline());
                }
                doc
            }
            Node::TypeAlias {
                export,
                name,
                params,
                body,
            } => {
                let body = (*body).to_ts();

                let doc = if *export {
                    RcDoc::text("export").append(RcDoc::space())
                } else {
                    RcDoc::nil()
                };

                let params_doc = match params {
                    list if list.len() == 0 => RcDoc::nil(),
                    list => {
                        let seperator = RcDoc::text(",").append(RcDoc::line());

                        let body = RcDoc::intersperse(
                            list.iter().map(|param| param.to_ts().group()),
                            seperator,
                        );

                        RcDoc::text("<")
                            .append(RcDoc::line_().append(body).append(RcDoc::line_()).nest(4))
                            .append(RcDoc::text(">"))
                            .group()
                    }
                };

                doc.append("type")
                    .append(RcDoc::space())
                    .append(name)
                    .append(params_doc)
                    .append(RcDoc::space())
                    .append("=")
                    .append(RcDoc::line().append(body).nest(4))
                    .group()
            }
            Node::Ident(ident) => RcDoc::text(ident),
            Node::Number(number) => RcDoc::text(number),
            Node::Primitive(primitive) => RcDoc::text(match primitive {
                PrimitiveType::Boolean => "boolean",
                PrimitiveType::Number => "number",
                PrimitiveType::String => "string",
            }),
            Node::String(string) => string_literal(string),
            Node::TemplateString(string) => RcDoc::text(string),
            Node::IfExpr(_cond, _then, _els) => {
                unreachable!("IfExpr should be desugared before this point");
            }
            Node::Access {
                lhs,
                rhs,
                is_dot: true,
            } => {
                let rhs = rhs
                    .as_ident()
                    .expect("rhs of dot access should be an ident");

                lhs.to_ts()
                    .append(RcDoc::text("["))
                    .append(smart_quote(rhs))
                    .append(RcDoc::text("]"))
                    .group()
            }
            Node::Access { lhs, rhs, .. } => lhs
                .to_ts()
                .append(RcDoc::text("["))
                .append(rhs.to_ts())
                .append(RcDoc::text("]"))
                .group(),
            Node::Error(_) => unreachable!("Error should be handled before this point"),
            Node::ObjectLiteral(props) => {
                let sep = RcDoc::text(",").append(RcDoc::space());

                let props = RcDoc::intersperse(props.iter().map(|prop| prop.to_ts()), sep);

                RcDoc::text("{").append(props).append(RcDoc::text("}"))
            }
            Node::Application(ident, params) => {
                let sep = RcDoc::text(",").append(RcDoc::space());

                let params = RcDoc::intersperse(params.iter().map(|param| param.to_ts()), sep);

                RcDoc::text(ident).append(RcDoc::text("<").append(params).append(RcDoc::text(">")))
            }
            Node::Tuple(items) => {
                let sep = RcDoc::text(",").append(RcDoc::space());

                let items = RcDoc::intersperse(items.iter().map(|item| item.to_ts()), sep);

                RcDoc::text("[").append(items).append(RcDoc::text("]"))
            }
            Node::Array(value) => {
                let doc = if value.is_bin_op() {
                    parens(value.to_ts())
                } else {
                    value.to_ts()
                };

                doc.append(RcDoc::text("[]"))
            }
            Node::Null => RcDoc::text("null"),
            Node::Undefined => RcDoc::text("undefined"),
            Node::Never => RcDoc::text("never"),
            Node::Any => RcDoc::text("any"),
            Node::Unknown => RcDoc::text("unknown"),
            Node::True => RcDoc::text("true"),
            Node::False => RcDoc::text("false"),

            Node::BinOp { lhs, op, rhs } => {
                fn fmt(v: &Node) -> RcDoc<()> {
                    match v {
                        Node::BinOp { .. } => parens(v.to_ts()),
                        _ => v.to_ts(),
                    }
                }

                let lhs = fmt(lhs);
                let rhs = fmt(rhs);

                let op = match op {
                    Op::Union => RcDoc::text("|"),
                    Op::Intersection => RcDoc::text("&"),
                };

                RcDoc::nil()
                    .append(lhs)
                    .append(RcDoc::space())
                    .append(op)
                    .append(RcDoc::space())
                    .append(rhs)
            }

            Node::Builtin { name, argument } => name.to_ts().append(" ").append(argument.to_ts()),

            Node::ExtendsExpr(lhs, rhs, then, els) => {
                let condition_doc = lhs
                    .to_ts()
                    .append(RcDoc::space())
                    .append("extends")
                    .append(RcDoc::space())
                    .append(rhs.to_ts());

                let then_doc = RcDoc::line()
                    .append("?")
                    .append(RcDoc::space())
                    .append(then.to_ts())
                    .nest(4);

                let else_doc = RcDoc::line()
                    .append(":")
                    .append(RcDoc::space())
                    .append(els.to_ts())
                    .nest(4);

                condition_doc.append(then_doc).append(else_doc)
            }
            Node::ExtendsBinOp {
                lhs,
                op: InfixOp::Extends,
                rhs,
            } => lhs
                .to_ts()
                .append(RcDoc::space())
                .append("extends")
                .append(RcDoc::space())
                .append(rhs.to_ts())
                .group(),

            Node::ExtendsBinOp { .. } => {
                unreachable!("ExtendsBinOp should be desugared before this point")
            }
            Node::ExtendsPrefixOp { .. } => {
                unreachable!("ExtendsPrefixOp should be desugared before this point")
            }
            Node::MatchExpr { .. } => {
                unreachable!("MatchExpr should be desugared before this point")
            }
            Node::CondExpr { .. } => {
                unreachable!("CondExpr should be desugared before this point")
            }
            Node::Statement(stmnt) => stmnt.to_ts().append(RcDoc::text(";")),
            Node::MappedType {
                index: key,
                iterable,
                remapped_as,
                readonly_mod,
                optional_mod,
                body,
            } => {
                let remapped_as_doc = match remapped_as {
                    Some(remapped_as) => RcDoc::space()
                        .append("as")
                        .append(RcDoc::space())
                        .append(remapped_as.to_ts()),
                    None => RcDoc::nil(),
                };

                let lhs_doc = RcDoc::nil()
                    .append(key)
                    .append(RcDoc::space())
                    .append("in")
                    .append(RcDoc::space())
                    .append(iterable.to_ts())
                    .append(remapped_as_doc)
                    .group();

                let rhs_doc = body.to_ts();

                let rhs_doc = RcDoc::line().append(rhs_doc).nest(4).group();

                let readonly_doc = match readonly_mod {
                    Some(MappingModifier::Add) => RcDoc::text("readonly"),
                    Some(MappingModifier::Remove) => RcDoc::text("-readonly"),
                    None => RcDoc::nil(),
                };

                let optional_doc = match optional_mod {
                    Some(MappingModifier::Add) => RcDoc::text("?"),
                    Some(MappingModifier::Remove) => RcDoc::text("-?"),
                    None => RcDoc::nil(),
                };

                let inner_doc = RcDoc::line()
                    .append(readonly_doc)
                    .append("[")
                    .append(lhs_doc)
                    .append("]")
                    .append(optional_doc)
                    .append(":")
                    .append(rhs_doc)
                    .append(RcDoc::line())
                    .nest(4)
                    .group();

                RcDoc::nil()
                    .append("{")
                    .append(inner_doc)
                    .append("}")
                    .group()
            }
            Node::LetExpr { .. } => {
                unreachable!("LetExpr should be desugared before this point")
            }
            Node::ImportStatement {
                import_clause,
                module,
            } => {
                let import_clause = import_clause.to_ts();

                RcDoc::text("import type")
                    .append(RcDoc::space())
                    .append(import_clause)
                    .append(RcDoc::space())
                    .append("from")
                    .append(RcDoc::space())
                    .append(string_literal(module))
            }
        }
    }
}

fn string_literal(string: &String) -> RcDoc<()> {
    RcDoc::text("\'")
        .append(RcDoc::text(string.replace("\'", "\\\'")))
        .append(RcDoc::text("\'"))
}

impl ToTypescript for TypeParameter {
    fn to_ts(&self) -> RcDoc<()> {
        let rest = if self.rest {
            RcDoc::text("...")
        } else {
            RcDoc::nil()
        };

        let constraint = match &self.constraint {
            Some(constraint) => RcDoc::space()
                .append("extends")
                .append(RcDoc::space())
                .append(constraint.to_ts()),
            None => RcDoc::nil(),
        };

        let default_value = match &self.default {
            Some(value) => RcDoc::space()
                .append("=")
                .append(RcDoc::space())
                .append(value.to_ts()),
            None => RcDoc::nil(),
        };

        RcDoc::nil()
            .append(rest)
            .append(self.name.clone())
            .append(constraint)
            .append(default_value)
    }
}

impl ToTypescript for BuiltInKeyword {
    fn to_ts(&self) -> RcDoc<()> {
        match self {
            BuiltInKeyword::Keyof => RcDoc::text("keyof"),
        }
    }
}

impl ToTypescript for Identifier {
    fn to_ts(&self) -> RcDoc<()> {
        RcDoc::text(self.0.clone())
    }
}

impl ToTypescript for ImportSpecifier {
    fn to_ts(&self) -> RcDoc<()> {
        let alias_doc = match &self.alias {
            Some(alias) => RcDoc::space()
                .append("as")
                .append(RcDoc::space())
                .append(alias.to_ts()),

            None => RcDoc::nil(),
        };

        self.module_export_name.to_ts().append(alias_doc)
    }
}

impl ToTypescript for ImportClause {
    fn to_ts(&self) -> RcDoc<()> {
        match self {
            ImportClause::Named(specifiers) => {
                let sep = RcDoc::text(",").append(RcDoc::line());

                let specifiers =
                    RcDoc::intersperse(specifiers.iter().map(ToTypescript::to_ts), sep);

                RcDoc::text("{")
                    .append(
                        RcDoc::nil()
                            .append(RcDoc::line())
                            .append(specifiers)
                            .append(RcDoc::line())
                            .nest(4),
                    )
                    .append(RcDoc::text("}"))
                    .group()
            }
            ImportClause::Namespace { alias } => RcDoc::text("*")
                .append(RcDoc::space())
                .append("as")
                .append(RcDoc::space())
                .append(alias.to_ts()),
        }
    }
}

impl ToTypescript for ObjectProperty {
    fn to_ts(&self) -> RcDoc<()> {
        let readonly = if self.readonly {
            RcDoc::text("readonly").append(RcDoc::space())
        } else {
            RcDoc::nil()
        };

        let optional = if self.optional {
            RcDoc::text("?")
        } else {
            RcDoc::nil()
        };

        let doc = RcDoc::nil();

        doc.append(readonly)
            .append(&self.key)
            .append(optional)
            .append(RcDoc::text(":"))
            .append(RcDoc::space())
            .append(self.value.to_ts())
    }
}

fn smart_quote(name: &str) -> RcDoc<()> {
    if name.chars().any(|c| c == '\"') {
        single_quote(RcDoc::text(name))
    } else {
        double_quote(RcDoc::text(name))
    }
}
