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
    /**
     * Simplification is a desugaring process that removes newtype specific language features,
     * replacing them with typescript compatible constructs.
     */
    fn simplify(&self) -> Self {
        self.transform(&|node| match node {
            // Replace all instances of `IfExpr` with `ExtendsExpr`
            Node::IfExpr(op, then, else_) => {
                let (lhs, rhs) = match *op.clone() {
                    Node::ExtendsBinOp { lhs, op, rhs } => (lhs, rhs),
                    _ => panic!("Expected ExtendsBinOp, found {op:#?}"),
                };

                let else_ = match else_ {
                    Some(v) => v.clone(),
                    None => Box::new(Node::Never),
                };

                Node::ExtendsExpr(lhs.clone(), rhs.clone(), then.clone(), else_.clone())
            }
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
    /**
     * Recursively transform a node and all of its children. There is no generalize
     * tree walk method in Rust to deal with ADTs, so we have to implement it manually.
     */
    fn transform<T>(&self, f: &T) -> Self
    where
        Self: Clone,
        T: Fn(&Self) -> Self,
    {
        let transform = |node: &Node| node.transform(f);

        let transform_and_box = |node: &Node| Box::new(node.transform(f));

        let transform_each =
            |nodes: &Vec<Node>| nodes.into_iter().map(transform).collect::<Vec<_>>();

        // For all nodes that are not a leaf node,
        // we need to recursively simplify
        let out = match self {
            Node::Program(vec) => Node::Program(transform_each(vec)),
            Node::TypeAlias(ident, type_params, expr) => Node::TypeAlias(
                ident.clone(),
                transform_each(type_params),
                Box::new(expr.transform(f)),
            ),
            Node::Tuple(vec) => Node::Tuple(transform_each(vec)),
            Node::Array(vec) => Node::Array(transform_and_box(vec)),
            Node::IfExpr(cond, then, els) => Node::IfExpr(
                transform_and_box(cond),
                transform_and_box(then),
                els.as_ref().map(|v| transform_and_box(v)),
            ),
            Node::BinOp { lhs, op, rhs } => Node::BinOp {
                lhs: transform_and_box(lhs),
                op: op.clone(),
                rhs: transform_and_box(rhs),
            },
            Node::ExtendsBinOp { lhs, op, rhs } => Node::ExtendsBinOp {
                lhs: transform_and_box(lhs),
                op: op.clone(),
                rhs: transform_and_box(rhs),
            },
            Node::ExtendsExpr(lhs, rhs, then, els) => Node::ExtendsExpr(
                transform_and_box(lhs),
                transform_and_box(rhs),
                transform_and_box(then),
                transform_and_box(els),
            ),
            Node::ObjectLiteral(props) => Node::ObjectLiteral(
                props
                    .into_iter()
                    .map(|prop| {
                        let mut p = prop.clone();
                        p.value = transform(&prop.value);
                        p
                    })
                    .collect(),
            ),
            Node::Application(name, args) => Node::Application(name.clone(), transform_each(args)),

            // Leaf nodes are not transformed
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
