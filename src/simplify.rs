use crate::ast::*;

pub trait Simplify {
    fn simplify(&self) -> Self;
}

pub trait Transform {
    fn transform<T>(&self, f: &T) -> Self
    where
        T: Fn(&Self) -> Self;
}

impl Simplify for Node {
    fn simplify(&self) -> Self {
        self.transform(&|node| match node {
            Node::IfExpr(_, _, _) => Node::Null,
            _ => node.clone(),
        })
    }
}

impl Transform for Expr {
    fn transform<T>(&self, f: &T) -> Self
    where
        Self: Clone,
        T: Fn(&Self) -> Self,
    {
        self.clone()
    }
}

impl Transform for Node {
    fn transform<T>(&self, f: &T) -> Self
    where
        Self: Clone,
        T: Fn(&Self) -> Self,
    {
        let trans = |node: &Node| node.transform(f);

        let transb = |node: &Node| Box::new(node.transform(f));

        let map = |nodes: &Vec<Node>| nodes.into_iter().map(trans).collect::<Vec<_>>();

        // For all nodes that are not a leaf node,
        // we need to recursively simplify
        let out = match self {
            Node::Program(vec) => Node::Program(map(vec)),
            Node::TypeAlias(ident, type_params, expr) => {
                Node::TypeAlias(ident.clone(), map(type_params), Box::new(expr.transform(f)))
            }
            Node::Tuple(vec) => Node::Tuple(map(vec)),
            Node::Array(vec) => Node::Array(transb(vec)),
            Node::IfExpr(cond, then, els) => {
                Node::IfExpr(transb(cond), transb(then), els.as_ref().map(|v| transb(v)))
            }
            Node::BinOp { lhs, op, rhs } => Node::BinOp {
                lhs: transb(lhs),
                op: op.clone(),
                rhs: transb(rhs),
            },
            Node::ExtendsBinOp { lhs, op, rhs } => Node::ExtendsBinOp {
                lhs: transb(lhs),
                op: op.clone(),
                rhs: transb(rhs),
            },
            Node::ExtendsExpr(lhs, rhs, then, els) => {
                Node::ExtendsExpr(transb(lhs), transb(rhs), transb(then), transb(els))
            }
            Node::ObjectLiteral(props) => Node::ObjectLiteral(
                props
                    .into_iter()
                    .map(|prop| {
                        let mut p = prop.clone();
                        p.value = trans(&prop.value);
                        p
                    })
                    .collect(),
            ),
            Node::Application(name, args) => Node::Application(name.clone(), map(args)),

            Node::None
            | Node::Error(_)
            | Node::Never
            | Node::Any
            | Node::Unknown
            | Node::Null
            | Node::Undefined
            | Node::False
            | Node::True
            | Node::Ident(_)
            | Node::Number(_)
            | Node::Primitive(_)
            | Node::String(_)
            | Node::TemplateString(_)
            | Node::Infer(_) => self.clone(),
        };

        f(&out)
    }
}
