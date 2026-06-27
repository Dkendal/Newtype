use std::rc::Rc;

use itertools::Itertools;

use crate::ast::{Ast, Bindings, UnionType};

impl Ast {
    pub fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        match &self {
            Ast::Access(expr) => Ast::Access(expr.map(f)),

            Ast::ApplyGeneric(expr) => Ast::ApplyGeneric(expr.map(f)),

            Ast::Array(node) => Ast::Array(Rc::new(f(node))),

            Ast::Builtin(expr) => Ast::Builtin(expr.map(f)),

            Ast::CondExpr(expr) => Ast::CondExpr(expr.map(f)),

            Ast::ExtendsInfixOp(expr) => Ast::ExtendsInfixOp(expr.map(f)),

            Ast::ExtendsExpr(expr) => Ast::ExtendsExpr(expr.map(f)),

            Ast::ExtendsPrefixOp(expr) => Ast::ExtendsPrefixOp(expr.map(f)),

            Ast::IfExpr(expr) => Ast::IfExpr(expr.map(f)),

            Ast::LetExpr(expr) => Ast::LetExpr(expr.map(f)),

            Ast::MappedType(expr) => Ast::MappedType(expr.map(f)),

            Ast::MatchExpr(expr) => Ast::MatchExpr(expr.map(f)),

            Ast::Path(expr) => Ast::Path(expr.map(f)),

            Ast::TypeLiteral(expr) => Ast::TypeLiteral(expr.map(f)),

            Ast::Program(expr) => Ast::Program(expr.map(f)),

            Ast::Statement(node) => Ast::Statement(f(node).into()),

            Ast::Tuple(expr) => Ast::Tuple(expr.map(f)),

            Ast::TypeAlias(expr) => Ast::TypeAlias(expr.map(f)),

            Ast::UnionType(expr) => Ast::UnionType(expr.map(f)),

            Ast::IntersectionType(expr) => Ast::IntersectionType(expr.map(f)),

            _ => self.clone(),
        }
    }

    pub fn simplify(&self) -> Self {
        let bindings: Bindings = Default::default();

        let identity = |node, ctx| (node, ctx);

        let (tree, _) = self.traverse(bindings, &identity, &|ast, ctx| {
            let span = ast.as_span();

            match ast {
                Ast::IfExpr(if_expr) => (if_expr.simplify(), ctx),
                Ast::MatchExpr(match_expr) => (match_expr.simplify(), ctx),
                Ast::CondExpr(cond_expr) => (cond_expr.simplify(), ctx),
                Ast::LetExpr(let_expr) => (let_expr.simplify(), ctx),
                Ast::Path(path) => (path.simplify(), ctx),
                Ast::UnionType(UnionType { types, .. }) => match types.as_slice() {
                    // Flatten nested union types (both)
                    [Ast::UnionType(UnionType {
                        types: lhs_types, ..
                    }), Ast::UnionType(UnionType {
                        types: rhs_types, ..
                    })] => {
                        let types = lhs_types
                            .clone()
                            .into_iter()
                            .chain(rhs_types.clone())
                            .collect();

                        let ast = Ast::UnionType(UnionType { types, span });

                        (ast, ctx)
                    }
                    // Flatten nested union types (rhs)
                    [lhs, Ast::UnionType(UnionType {
                        types: rhs_types, ..
                    })] => {
                        let mut types = rhs_types.clone();

                        types.push(lhs.clone());

                        let ast = Ast::UnionType(UnionType { types, span });

                        (ast, ctx)
                    }
                    // Flatten nested union types (lhs)
                    [Ast::UnionType(UnionType {
                        types: lhs_types, ..
                    }), rhs] => {
                        let mut types = lhs_types.clone();

                        types.push(rhs.clone());

                        let ast = Ast::UnionType(UnionType { types, span });

                        (ast, ctx)
                    }
                    // Move all intersection to the right
                    types => {
                        let types = types
                            .iter()
                            .sorted_by(|a, b| a.is_intersection().cmp(&b.is_intersection()))
                            .cloned()
                            .collect_vec();

                        let ast = Ast::UnionType(UnionType { types, span });

                        (ast, ctx)
                    }
                },
                _ => (ast, ctx),
            }
        });
        tree
    }

    pub fn prewalk<Context, F>(&self, ctx: Context, pre: &F) -> (Self, Context)
    where
        Context: Clone,
        F: Fn(Self, Context) -> (Self, Context),
    {
        self.traverse(ctx, pre, &|n, c| (n, c))
    }

    pub fn postwalk<Context, F>(&self, ctx: Context, post: &F) -> (Self, Context)
    where
        Context: Clone,
        F: Fn(Self, Context) -> (Self, Context),
    {
        self.traverse(ctx, &|n, c| (n, c), post)
    }

    pub fn traverse<Context, Pre, Post>(
        &self,
        ctx: Context,
        pre: &Pre,
        post: &Post,
    ) -> (Self, Context)
    where
        Context: Clone,
        Pre: Fn(Self, Context) -> (Self, Context),
        Post: Fn(Self, Context) -> (Self, Context),
    {
        let ast = self.clone();

        let (ast, ctx) = pre(ast, ctx);

        let ast = ast.map(|ast| ast.traverse(ctx.clone(), pre, post).0);

        let (ast, acc) = post(ast, ctx);

        (ast, acc)
    }
}
