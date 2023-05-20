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
                    doc = doc
                        .append(stmnt.to_ts())
                        .append(";")
                        .append(RcDoc::hardline());
                }
                doc
            }
            Node::TypeAlias(name, params, body) => {
                let body = (*body).to_ts();

                RcDoc::text("type")
                    .append(RcDoc::space())
                    .append(name)
                    .append(match params {
                        list if list.len() == 0 => RcDoc::nil(),
                        list => {
                            let seperator = RcDoc::text(",").append(RcDoc::space());

                            let body = RcDoc::intersperse(
                                list.iter().map(|param| param.to_ts()),
                                seperator,
                            );

                            RcDoc::text("<").append(body).append(RcDoc::text(">"))
                        }
                    })
                    .append(RcDoc::space())
                    .append("=")
                    .append(RcDoc::space())
                    .append(body)
            }
            Node::Ident(ident) => RcDoc::text(ident),
            Node::Number(number) => RcDoc::text(number),
            Node::Primitive(primitive) => RcDoc::text(match primitive {
                Primitive::Boolean => "boolean",
                Primitive::Number => "number",
                Primitive::String => "string",
            }),
            Node::String(string) => RcDoc::text(string),
            Node::TemplateString(_) => todo!(),
            Node::IfExpr(cond, then, els) => {
                unreachable!();
            }
            // (*cond)
            // .to_ts()
            // .append(RcDoc::space())
            // .append("?")
            // .append(RcDoc::space())
            // .append(then.to_ts())
            // .append(RcDoc::space())
            // .append(":")
            // .append(RcDoc::space())
            // .append(match els {
            //     Some(els) => els.to_ts(),
            //     None => RcDoc::text("never"),
            // }),
            Node::None => todo!(),
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
            Node::ExtendsExpr(lhs, rhs, then, els) => lhs
                .to_ts()
                .append(RcDoc::space())
                .append("extends")
                .append(RcDoc::space())
                .append(rhs.to_ts())
                .append(RcDoc::space())
                .append("?")
                .append(RcDoc::space())
                .append(then.to_ts())
                .append(RcDoc::space())
                .append(":")
                .append(RcDoc::space())
                .append(els.to_ts()),
            Node::Infer(ident) => RcDoc::text("infer").append(RcDoc::space()).append(ident),
            Node::ExtendsBinOp { .. } => unreachable!(),
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
