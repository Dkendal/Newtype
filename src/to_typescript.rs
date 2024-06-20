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
                        let seperator = RcDoc::text(",").append(RcDoc::space());

                        let body =
                            RcDoc::intersperse(list.iter().map(|param| param.to_ts()), seperator);

                        RcDoc::text("<").append(body).append(RcDoc::text(">"))
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
            Node::String(string) => RcDoc::text(string),
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
            Node::Error(_) => todo!(),
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
        }
    }
}

impl ToTypescript for BuiltInKeyword {
    fn to_ts(&self) -> RcDoc<()> {
        match self {
            BuiltInKeyword::Keyof => RcDoc::text("keyof"),
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
