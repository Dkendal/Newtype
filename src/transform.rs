use itertools::Itertools;

use crate::ast::*;

pub trait Transform {
    fn transform<T>(&self, f: &T) -> Self
    where
        T: Fn(&Self) -> Self;
}

impl Transform for Expr {
    fn transform<T>(&self, _f: &T) -> Self
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
            // Leaf nodes are not transformed
            Node::Error { .. }
            | Node::Never
            | Node::Any
            | Node::Unknown
            | Node::Null
            | Node::Undefined
            | Node::False
            | Node::True
            | Node::Ident { .. }
            | Node::Number { .. }
            | Node::Primitive { .. }
            | Node::String { .. }
            | Node::TemplateString { .. } => self.clone(),

            // For all other nodes, we recursively transform
            Node::Program(vec) => Node::Program(transform_each(vec)),
            Node::TypeAlias {
                export,
                name,
                params,
                body,
            } => {
                let params = params
                    .iter()
                    .map(|param| {
                        let mut param = param.clone();
                        param.default = param.default.map(|d| Box::new(d.transform(f)));
                        param.constraint = param.constraint.map(|d| Box::new(d.transform(f)));
                        param
                    })
                    .collect_vec();

                Node::TypeAlias {
                    export: *export,
                    name: name.clone(),
                    params,
                    body: Box::new(body.transform(f)),
                }
            }
            Node::Tuple(vec) => Node::Tuple(transform_each(vec)),
            Node::Array(vec) => Node::Array(transform_and_box(vec)),
            Node::Access { lhs, rhs, is_dot } => Node::Access {
                lhs: transform_and_box(lhs),
                rhs: transform_and_box(rhs),
                is_dot: *is_dot,
            },
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

            Node::ExtendsPrefixOp { op, value } => Node::ExtendsPrefixOp {
                op: op.clone(),
                value: transform_and_box(value),
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

            Node::MatchExpr { value, arms, else_ } => {
                let value = transform_and_box(value);

                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        let mut a = arm.clone();
                        a.pattern = transform(&arm.pattern);
                        a.body = transform(&arm.body);
                        a
                    })
                    .collect();

                let else_ = Box::new(transform(else_));

                Node::MatchExpr { value, arms, else_ }
            }

            Node::CondExpr { arms, else_ } => {
                let arms = arms
                    .into_iter()
                    .map(|arm| {
                        let mut a = arm.clone();
                        a.condition = transform(&arm.condition);
                        a.body = transform(&arm.body);
                        a
                    })
                    .collect();

                let else_ = Box::new(transform(else_));

                Node::CondExpr { arms, else_ }
            }

            Node::Builtin { name, argument } => Node::Builtin {
                name: name.clone(),
                argument: transform_and_box(argument),
            },
            Node::Statement(node) => Node::Statement(Box::new(node.transform(f))),

            Node::MappedType {
                index,
                iterable,
                remapped_as,
                readonly_mod,
                optional_mod,
                body,
            } => Node::MappedType {
                index: index.clone(),
                iterable: Box::new(iterable.transform(f)),
                remapped_as: remapped_as.clone(),
                readonly_mod: readonly_mod.clone(),
                optional_mod: optional_mod.clone(),
                body: Box::new(body.transform(f)),
            },
        };

        f(&out)
    }
}
