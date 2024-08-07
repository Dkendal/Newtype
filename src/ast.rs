use std::{collections::HashMap, fmt::Display, rc::Rc};

use cond_expr::CondExpr;
use derivative::Derivative;
use if_expr::IfExpr;
use itertools::Itertools;
use let_expr::LetExpr;
use match_expr::MatchExpr;
use newtype_macros_lib::ast_node;
use pretty::RcDoc as D;
use serde_derive::Serialize;

use crate::{
    extends_result::ExtendsResult,
    parser::{Pair, ParserError, Rule},
    pretty::{parens, string_literal, surround},
    runtime::{self, builtin},
    typescript,
};

pub(crate) mod errors;
pub(crate) mod macros;

pub type Bindings = HashMap<String, Ast>;

#[ast_node]
pub struct Path {
    pub segments: Vec<Ast>,
}

impl Path {
    pub fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        Self {
            span: self.span,
            segments: self.segments.iter().map(f).collect(),
        }
    }

    fn simplify(&self) -> Ast {
        let mut acc = vec![];

        for seg in self.segments.iter() {
            if let Ast::Path(path) = seg {
                acc.extend(path.segments.clone());
            } else {
                acc.push(seg.clone());
            }
        }

        Ast::Path(Path {
            span: self.span,
            segments: acc,
        })
    }
}

#[ast_node]
pub struct ExtendsExpr {
    pub lhs: Rc<Ast>,
    pub rhs: Rc<Ast>,
    pub then_branch: Rc<Ast>,
    pub else_branch: Rc<Ast>,
}

impl ExtendsExpr {
    pub fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        let mut expr = self.clone();

        expr.lhs = Rc::new(f(&self.lhs));
        expr.rhs = Rc::new(f(&self.rhs));
        expr.then_branch = Rc::new(f(&self.then_branch));
        expr.else_branch = Rc::new(f(&self.else_branch));
        expr
    }

    pub fn new(
        span: Span,
        lhs: Rc<Ast>,
        rhs: Rc<Ast>,
        then_branch: Rc<Ast>,
        else_branch: Rc<Ast>,
    ) -> Self {
        if !lhs.is_typescript_feature() {
            dbg!(&lhs);
            unreachable!("value must be desugared before this point");
        }
        if !rhs.is_typescript_feature() {
            dbg!(&rhs);
            unreachable!("value must be desugared before this point");
        }
        if !then_branch.is_typescript_feature() {
            dbg!(&then_branch);
            unreachable!("value must be desugared before this point");
        }
        if !else_branch.is_typescript_feature() {
            dbg!(&else_branch);
            unreachable!("value must be desugared before this point");
        }

        Self {
            span,
            lhs,
            rhs,
            then_branch,
            else_branch,
        }
    }
}

#[ast_node]
pub struct Tuple {
    pub items: Vec<Ast>,
}

impl Tuple {
    pub fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        Self {
            items: self.items.iter().map(f).collect(),
            span: self.span,
        }
    }
}

#[ast_node]
pub struct ApplyGeneric {
    pub receiver: Rc<Ast>,
    pub args: Vec<Ast>,
}

impl ApplyGeneric {
    fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        let mut expr = self.clone();
        expr.receiver = f(&self.receiver).into();
        expr.args = self.args.iter().map(f).collect();
        expr
    }
}

impl typescript::Pretty for ApplyGeneric {
    fn to_ts(&self) -> D<()> {
        let sep = D::text(",").append(D::space());

        let generic_inner = D::intersperse(self.args.iter().map(|param| param.to_ts()), sep);

        let generic_params = D::text("<").append(generic_inner).append(D::text(">"));

        self.receiver.to_ts().append(generic_params)
    }
}

#[ast_node]
pub struct MappedType {
    pub index: String,
    pub iterable: Rc<Ast>,
    pub remapped_as: Option<Rc<Ast>>,
    pub readonly_mod: Option<MappingModifier>,
    pub optional_mod: Option<MappingModifier>,
    pub body: Rc<Ast>,
}

impl MappedType {
    fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        let mut expr = self.clone();
        expr.iterable = Rc::new(f(&self.iterable));
        expr.remapped_as = self.remapped_as.as_ref().map(|x| Rc::new(f(x)));
        expr.body = f(&self.body).into();
        expr
    }
}

#[ast_node]
pub struct TypeLiteral {
    pub properties: Vec<ObjectProperty>,
}

impl TypeLiteral {
    pub fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        Self {
            properties: self
                .properties
                .clone()
                .into_iter()
                .map(|prop| prop.map(|ty| f(ty)))
                .collect(),
            span: self.span,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.properties.is_empty()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, ObjectProperty> {
        self.properties.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, ObjectProperty> {
        self.properties.iter_mut()
    }
}

impl typescript::Pretty for TypeLiteral {
    fn to_ts(&self) -> D<()> {
        let props = &self.properties;

        let sep = D::text(",").append(D::line());

        let props = D::intersperse(props.iter().map(|prop| prop.to_ts()), sep);

        D::nil()
            .append("{")
            .append(D::line_())
            .append(props.nest(4))
            .append(D::line_())
            .append(D::text("}"))
            .group()
    }
}

#[ast_node]
pub struct Interface {
    pub export: bool,
    pub name: String,
    pub extends: Option<String>,
    pub params: Vec<TypeParameter>,
    pub definition: Vec<ObjectProperty>,
}

impl typescript::Pretty for Interface {
    fn to_ts(&self) -> D<()> {
        let Interface {
            export,
            name,
            extends,
            params,
            definition,
            ..
        } = self;

        let doc = if *export {
            D::text("export").append(D::space())
        } else {
            D::nil()
        };

        let extends = match extends {
            Some(extends) => D::space()
                .append("extends")
                .append(D::space())
                .append(extends)
                .append(D::space()),
            None => D::nil(),
        };

        let params_doc = match params {
            list if list.is_empty() => D::nil(),
            list => {
                let seperator = D::text(",").append(D::line());

                let params_body =
                    D::intersperse(list.iter().map(|param| param.to_ts().group()), seperator);

                D::text("<")
                    .append(D::line_().append(params_body).append(D::line_()).nest(4))
                    .append(D::text(">"))
                    .group()
            }
        };

        let body = if definition.is_empty() {
            D::text("{}")
        } else {
            let body = definition.iter().map(|p| p.to_ts());

            let body = D::intersperse(body, D::text(";").append(D::hardline())).append(";");

            let body = D::hardline().append(body).nest(4);

            D::nil()
                .append("{")
                .append(body)
                .append(D::hardline())
                .append("}")
        };

        doc.append("interface")
            .append(D::space())
            .append(name)
            .append(params_doc)
            .append(extends)
            .append(D::space())
            .append(body)
            .group()
    }
}

#[ast_node]
pub struct UnitTest {
    pub name: String,
    pub body: Vec<Ast>,
}

#[ast_node]
pub struct MacroCall {
    pub name: String,
    pub args: Vec<Ast>,
}

impl MacroCall {
    fn eval(&self) -> Ast {
        let name = self.name.strip_suffix("!").unwrap();

        match name {
            "dbg!" => match self.args.as_slice() {
                [node] => builtin::dbg(node.to_owned()),
                _ => panic!("dbg! expects exactly one argument"),
            },
            "assert_equal" => match self.args.as_slice() {
                [lhs, rhs] => builtin::assert_equal(lhs.to_owned(), rhs.to_owned()),
                _ => panic!("assert_equal! expects exactly two arguments"),
            },
            "unquote" => match self.args.as_slice() {
                [node] => builtin::unquote(node.to_owned()),
                _ => panic!("unquote! expects exactly one argument"),
            },
            id => unimplemented!("macro {} not implemented", id),
        }
    }
}

#[ast_node]
pub struct FunctionType {
    pub params: Vec<Parameter>,
    pub return_type: Rc<Ast>,
}

impl typescript::Pretty for FunctionType {
    fn to_ts(&self) -> D<()> {
        let sep = D::text(",").append(D::space());

        let params = D::intersperse(self.params.iter().map(|param| param.to_ts()), sep);

        let params = D::text("(").append(params).append(D::text(")")).group();

        let return_type = self.return_type.to_ts();

        params
            .append(D::space())
            .append("=>")
            .append(D::space())
            .append(return_type)
    }
}

#[ast_node]
pub struct Parameter {
    pub ellipsis: bool,
    pub name: String,
    pub kind: Ast,
}

impl typescript::Pretty for Parameter {
    fn to_ts(&self) -> D<()> {
        let kind = self.kind.to_ts();

        if self.ellipsis {
            D::text("...")
        } else {
            D::nil()
        }
        .append(self.name.clone())
        .append(":")
        .append(D::space())
        .append(kind)
    }
}

#[ast_node]
pub struct Access {
    pub lhs: Rc<Ast>,
    pub rhs: Rc<Ast>,
    pub is_dot: bool,
}

impl Access {
    fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        Self {
            lhs: f(&self.lhs).into(),
            rhs: f(&self.rhs).into(),
            is_dot: self.is_dot,
            span: self.span,
        }
    }
}

#[ast_node]
pub struct IntersectionType {
    pub types: Vec<Ast>,
}

impl IntersectionType {
    fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        Self {
            types: self.types.iter().map(f).collect(),
            span: self.span,
        }
    }
}

#[ast_node]
pub struct Builtin {
    pub name: BuiltinKeyword,
    pub argument: Rc<Ast>,
}

impl Builtin {
    fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        Self {
            argument: f(&self.argument).into(),
            ..self.clone()
        }
    }
}

#[ast_node]
pub struct UnionType {
    pub types: Vec<Ast>,
}

impl UnionType {
    fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        Self {
            types: self.types.iter().map(f).collect(),
            span: self.span,
        }
    }
}

#[ast_node]
pub struct ExtendsInfixOp {
    pub lhs: Rc<Ast>,
    pub op: InfixOp,
    pub rhs: Rc<Ast>,
}

impl ExtendsInfixOp {
    fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        Self {
            lhs: f(&self.lhs).into(),
            rhs: f(&self.rhs).into(),
            ..self.clone()
        }
    }
}

#[ast_node]
pub struct ExtendsPrefixOp {
    pub op: PrefixOp,
    pub value: Rc<Ast>,
}

impl ExtendsPrefixOp {
    fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        Self {
            value: f(&self.value).into(),
            ..self.clone()
        }
    }
}

#[ast_node]
pub struct ImportStatement {
    pub import_clause: ImportClause,
    pub module: String,
}

#[ast_node]
pub struct TypeAlias {
    pub export: bool,
    pub name: Ident,
    pub params: Vec<TypeParameter>,
    pub body: Rc<Ast>,
}

impl TypeAlias {
    pub fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        Self {
            body: f(&self.body).into(),
            params: self.params.iter().map(|ty| ty.map(&f)).collect(),
            ..self.clone()
        }
    }
}

#[ast_node]
pub struct Program {
    pub statements: Vec<Ast>,
}

impl Program {
    pub fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        Self {
            statements: self.statements.iter().map(f).collect(),
            span: self.span,
        }
    }
}

#[ast_node]
pub enum Ast {
    #[serde(rename(serialize = "."))]
    Access(Access),
    #[serde(rename(serialize = "macro"))]
    MacroCall(MacroCall),
    #[serde(rename(serialize = "apply"))]
    ApplyGeneric(ApplyGeneric),
    Array(Rc<Ast>),
    #[serde(rename(serialize = "|"))]
    UnionType(UnionType),
    #[serde(rename(serialize = "&"))]
    IntersectionType(IntersectionType),
    Builtin(Builtin),
    CondExpr(CondExpr),
    ExtendsInfixOp(ExtendsInfixOp),
    ExtendsExpr(ExtendsExpr),
    Infer(Rc<Ast>),
    ExtendsPrefixOp(ExtendsPrefixOp),
    Ident(Ident),
    #[serde(rename = "if")]
    IfExpr(IfExpr),
    #[serde(rename = "import")]
    ImportStatement(ImportStatement),
    #[serde(rename = "let")]
    LetExpr(LetExpr),
    MappedType(MappedType),
    #[serde(rename = "match")]
    MatchExpr(MatchExpr),
    #[serde(rename = "::")]
    Path(Path),
    #[serde(rename = "number")]
    TypeNumber(TypeNumber),
    TypeLiteral(TypeLiteral),
    #[ast_node(span)]
    Primitive(PrimitiveType),
    Program(Program),
    Statement(Rc<Ast>),
    UnitTest(UnitTest),
    TypeString(TypeString),
    TemplateString(TemplateString),
    Tuple(Tuple),
    #[serde(rename(serialize = "type"))]
    TypeAlias(TypeAlias),
    #[serde(rename(serialize = "never"))]
    #[ast_node(span)]
    NeverKeyword(),
    #[serde(rename(serialize = "unknown"))]
    #[ast_node(span)]
    TrueKeyword(),
    #[ast_node(span)]
    FalseKeyword(),
    Interface(Interface),
    FunctionType(FunctionType),
    #[ast_node(span)]
    UnknownKeyword(),
    #[serde(rename = "any")]
    #[ast_node(span)]
    AnyKeyword(),
    #[ast_node(span)]
    NoOp(),
}

impl Ast {
    pub fn to_sexp(&self) -> serde_lexpr::Result<serde_lexpr::Value> {
        serde_lexpr::to_value(self)
    }

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

    /// Operators that the `not` prefix operator can be applied to.
    pub fn is_compatible_with_not_prefix_op(&self) -> bool {
        match self {
            Ast::ExtendsPrefixOp(ExtendsPrefixOp { op, .. }) if op.is_not() => true,
            Ast::ExtendsInfixOp(ExtendsInfixOp { .. }) => true,
            _ => false,
        }
    }

    pub fn is_top_type(&self) -> bool {
        matches!(self, Ast::AnyKeyword(_) | Ast::UnknownKeyword(_))
    }

    pub fn is_empty_object(&self) -> bool {
        matches!(self, Ast::TypeLiteral(TypeLiteral { properties, .. }) if properties.is_empty())
    }

    pub fn is_nullish(&self) -> bool {
        matches!(
            self,
            Ast::Primitive(
                PrimitiveType::Undefined | PrimitiveType::Null | PrimitiveType::Void,
                _
            )
        )
    }

    fn is_non_nullish(&self) -> bool {
        !self.is_nullish()
    }

    fn is_non_primitive(&self) -> bool {
        !self.is_primitive()
    }

    fn is_object_interface(&self) -> bool {
        self.has_identifier("Object")
    }

    fn has_identifier(&self, test: &str) -> bool {
        matches!(self, Ast::Ident(Ident{name, ..}) if name == test)
    }

    fn is_primitive(&self) -> bool {
        matches!(self, Ast::Primitive(_, _))
    }

    pub fn is_bottom_type(&self) -> bool {
        matches!(self, Ast::NeverKeyword(_))
    }

    pub fn get_primitive_type(&self) -> Option<PrimitiveType> {
        type P = PrimitiveType;

        let value = match self {
            Ast::TypeString(_) => P::String,

            Ast::TypeNumber(_) => P::Number,

            Ast::TrueKeyword(_) => P::Boolean,

            Ast::FalseKeyword(_) => P::Boolean,

            Ast::TypeLiteral(_) => P::Object,

            Ast::Ident(Ident { name, .. }) => match name.as_str() {
                "String" => P::String,
                "Number" => P::Number,
                "Boolean" => P::Boolean,
                "Object" => P::Object,
                "Symbol" => P::Symbol,
                "BigInt" => P::BigInt,
                _ => return None,
            },
            _ => return None,
        };

        Some(value)
    }

    pub fn is_object_wrapper(&self) -> bool {
        matches!(self, Ast::Ident(Ident{name, ..}) if matches!(name.as_str(),
                "Boolean" | "Number" | "String" | "Object" | "Symbol" | "BigInt"
        ))
    }

    pub fn is_object_wrapper_for(&self, object_wrapper_name: &str) -> bool {
        matches!(self, Ast::Ident(Ident{name, ..}) if name.as_str() == object_wrapper_name)
    }

    pub fn is_string_object_wrapper(&self) -> bool {
        self.is_object_wrapper_for("String")
    }

    pub fn is_number_object_wrapper(&self) -> bool {
        self.is_object_wrapper_for("Number")
    }

    pub fn is_boolean_object_wrapper(&self) -> bool {
        self.is_object_wrapper_for("Boolean")
    }

    pub fn is_object_object_wrapper(&self) -> bool {
        self.is_object_wrapper_for("Object")
    }

    pub fn is_symbol_object_wrapper(&self) -> bool {
        self.is_object_wrapper_for("Symbol")
    }

    pub fn is_bigint_object_wrapper(&self) -> bool {
        self.is_object_wrapper_for("BigInt")
    }

    pub fn get_object_wrapper(&self) -> Option<Ast> {
        let name = match self {
            Ast::Primitive(p, _) => {
                type P = PrimitiveType;

                match p {
                    P::Boolean => "Boolean",
                    P::Number => "Number",
                    P::String => "String",
                    P::Object => "Object",
                    P::Symbol => "Symbol",
                    P::BigInt => "BigInt",
                    P::Void | P::Undefined | P::Null => return None,
                }
            }
            ast if ast.is_object_interface() => return None,
            _ => "Object",
        };
        Some(Ast::Ident(Ident {
            name: name.to_string(),
            span: self.as_span(),
        }))
    }

    /// Anything that returns true is a feature that has a direct equivalent in TypeScript.
    /// Anything that's false is a feature that needs to be desugared.
    pub fn is_typescript_feature(&self) -> bool {
        matches!(
            self,
            Ast::IntersectionType { .. }
                | Ast::ExtendsExpr(_)
                | Ast::Ident(_)
                | Ast::Infer { .. }
                | Ast::ImportStatement { .. }
                | Ast::MappedType(_)
                | Ast::Path(_)
                | Ast::NeverKeyword(_)
                | Ast::TypeNumber(_)
                | Ast::TypeLiteral(_)
                | Ast::Primitive(..)
                | Ast::Program(_)
                | Ast::Statement(_)
                | Ast::TypeString(_)
                | Ast::TemplateString(_)
                | Ast::Tuple(_)
                | Ast::UnknownKeyword(_)
                | Ast::Interface(_)
                | Ast::TrueKeyword(_)
                | Ast::FalseKeyword(_)
                | Ast::UnionType { .. }
                | Ast::Access { .. }
                | Ast::AnyKeyword(_)
                | Ast::ApplyGeneric(_)
                | Ast::Array(_)
                | Ast::FunctionType(_)
                | Ast::Builtin { .. }
        )
    }

    /// Returns `true` if the node is [`ExtendsPrefixOp`].
    ///
    /// [`ExtendsPrefixOp`]: Ast::ExtendsPrefixOp
    #[must_use]
    pub fn is_extends_prefix_op(&self) -> bool {
        matches!(self, Self::ExtendsPrefixOp { .. })
    }

    /// Returns `true` if the ast is [`ExtendsInfixOp`].
    ///
    /// [`ExtendsInfixOp`]: Ast::ExtendsInfixOp
    #[must_use]
    pub fn is_extends_infix_op(&self) -> bool {
        matches!(self, Self::ExtendsInfixOp { .. })
    }

    /// Returns `true` if the ast is [`InfixOp`].
    ///
    /// [`InfixOp`]: Ast::InfixOp
    #[must_use]
    pub fn is_set_op(&self) -> bool {
        matches!(self, Self::UnionType { .. } | Self::IntersectionType { .. })
    }

    #[must_use]
    pub fn is_union(&self) -> bool {
        matches!(self, Self::UnionType { .. })
    }

    #[must_use]
    pub fn is_intersection(&self) -> bool {
        matches!(self, Self::IntersectionType { .. })
    }

    #[must_use]
    pub fn is_ident(&self) -> bool {
        matches!(self, Self::Ident(_))
    }

    pub fn as_ident(&self) -> Option<&Ident> {
        if let Self::Ident(v) = self {
            Some(v)
        } else {
            None
        }
    }

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

    pub fn as_span(&self) -> Span {
        match self {
            Ast::Access(x) => x.span,
            Ast::AnyKeyword(x) => *x,
            Ast::ApplyGeneric(x) => x.span,
            Ast::Array(ast) => ast.as_span(),
            Ast::Builtin(x) => x.span,
            Ast::CondExpr(x) => x.span,
            Ast::ExtendsExpr(x) => x.span,
            Ast::ExtendsInfixOp(x) => x.span,
            Ast::ExtendsPrefixOp(x) => x.span,
            Ast::FalseKeyword(x) => *x,
            Ast::FunctionType(x) => x.span,
            Ast::Ident(x) => x.span,
            Ast::IfExpr(x) => x.span,
            Ast::ImportStatement(x) => x.span,
            Ast::Infer(ast) => ast.as_span(),
            Ast::Interface(x) => x.span,
            Ast::IntersectionType(x) => x.span,
            Ast::LetExpr(x) => x.span,
            Ast::MacroCall(x) => x.span,
            Ast::MappedType(x) => x.span,
            Ast::MatchExpr(x) => x.span,
            Ast::NeverKeyword(x) => *x,
            Ast::NoOp(x) => *x,
            Ast::TypeNumber(x) => x.span,
            Ast::Path(x) => x.span,
            Ast::Primitive(_, x) => *x,
            Ast::Program(x) => x.span,
            Ast::Statement(ast) => ast.as_span(),
            Ast::TypeString(x) => x.span,
            Ast::TemplateString(x) => x.span,
            Ast::TrueKeyword(x) => *x,
            Ast::Tuple(x) => x.span,
            Ast::TypeAlias(x) => x.span,
            Ast::TypeLiteral(x) => x.span,
            Ast::UnionType(x) => x.span,
            Ast::UnitTest(x) => x.span,
            Ast::UnknownKeyword(x) => *x,
        }
    }
}

#[ast_node(transparent)]
pub struct TypeNumber {
    pub ty: String,
}

#[ast_node(transparent)]
pub struct TypeString {
    pub ty: String,
}

#[ast_node(transparent)]
pub struct TemplateString {
    pub ty: String,
}

#[derive(Debug, Clone, Copy, Eq, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn merge(&self, other: &Self) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }

    pub fn as_pest<'a>(&self, input: &'a str) -> pest::Span<'a> {
        pest::Span::new(input, self.start, self.end).unwrap()
    }

    pub fn as_parsing_error(
        &self,
        input: &str,
        positives: Vec<Rule>,
        negatives: Vec<Rule>,
    ) -> pest::error::Error<Rule> {
        let span = self.as_pest(input);
        let variant = pest::error::ErrorVariant::ParsingError {
            positives,
            negatives,
        };
        pest::error::Error::new_from_span(variant, span)
    }

    pub(crate) fn as_custom_error(
        &self,
        input: &str,
        message: String,
    ) -> pest::error::Error<Rule> {
        let span = self.as_pest(input);
        let variant = pest::error::ErrorVariant::CustomError { message };
        pest::error::Error::new_from_span(variant, span)
    }
}

impl From<Vec<Pair<'_>>> for Span {
    fn from(list: Vec<Pair<'_>>) -> Self {
        let start = list.iter().map(|p| p.as_span().start()).min().unwrap_or(0);
        let end = list.iter().map(|p| p.as_span().end()).max().unwrap_or(0);
        Self { start, end }
    }
}

impl From<&Pair<'_>> for Span {
    fn from(value: &Pair<'_>) -> Self {
        value.as_span().into()
    }
}

impl From<Pair<'_>> for Span {
    fn from(value: Pair<'_>) -> Self {
        (&value).into()
    }
}

impl From<pest::Span<'_>> for Span {
    fn from(value: pest::Span<'_>) -> Self {
        Self {
            start: value.start().to_owned(),
            end: value.end().to_owned(),
        }
    }
}

impl PartialEq for Span {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl From<Rc<Ast>> for Ast {
    fn from(value: Rc<Ast>) -> Self {
        (*value).clone()
    }
}

impl From<&Rc<Ast>> for Ast {
    fn from(value: &Rc<Ast>) -> Self {
        (**value).clone()
    }
}

impl From<if_expr::IfExpr> for Ast {
    fn from(v: if_expr::IfExpr) -> Self {
        Self::IfExpr(v)
    }
}

impl From<match_expr::MatchExpr> for Ast {
    fn from(v: match_expr::MatchExpr) -> Self {
        Self::MatchExpr(v)
    }
}

impl From<cond_expr::CondExpr> for Ast {
    fn from(v: cond_expr::CondExpr) -> Self {
        Self::CondExpr(v)
    }
}

impl From<Rc<Ast>> for Box<Ast> {
    fn from(value: Rc<Ast>) -> Self {
        Box::new((*value).clone())
    }
}

impl From<&Rc<Ast>> for Box<Ast> {
    fn from(value: &Rc<Ast>) -> Self {
        Box::new((**value).clone())
    }
}

impl typescript::Pretty for Ast {
    fn to_ts(&self) -> D<()> {
        match self {
            Ast::Program(Program { statements, .. }) => {
                let mut doc = D::nil();
                for stmnt in statements {
                    doc = doc
                        .append(stmnt.to_ts())
                        .append(D::hardline())
                        .append(D::hardline());
                }
                doc
            }
            Ast::TypeAlias(TypeAlias {
                export,
                name,
                params,
                body,
                ..
            }) => {
                let body = (*body).to_ts();

                let doc = if *export {
                    D::text("export").append(D::space())
                } else {
                    D::nil()
                };

                let params_doc = match params {
                    list if list.is_empty() => D::nil(),
                    list => {
                        let seperator = D::text(",").append(D::line());

                        let body = D::intersperse(
                            list.iter().map(|param| param.to_ts().group()),
                            seperator,
                        );

                        D::text("<")
                            .append(D::line_().append(body).append(D::line_()).nest(4))
                            .append(D::text(">"))
                            .group()
                    }
                };

                doc.append("type")
                    .append(D::space())
                    .append(name.pretty())
                    .append(params_doc)
                    .append(D::space())
                    .append("=")
                    .append(D::line().append(body).nest(4))
                    .group()
            }
            Ast::Ident(identifier) => identifier.pretty(),
            Ast::TypeNumber(inner) => D::text(inner.ty.clone()),
            Ast::Primitive(primitive, _) => D::text(primitive.to_string()),
            Ast::TypeString(inner) => string_literal(inner.ty.as_str()),
            Ast::TemplateString(inner) => D::text(inner.ty.clone()),
            Ast::IfExpr(..) => {
                unreachable!("IfExpr should be desugared before this point");
            }
            Ast::Access(Access {
                lhs,
                rhs,
                is_dot: true,
                ..
            }) => {
                let rhs = rhs
                    .as_ident()
                    .expect("rhs of dot access should be an ident");

                lhs.to_ts()
                    .append(D::text("["))
                    .append(string_literal(rhs.name.as_str()))
                    .append(D::text("]"))
                    .group()
            }
            Ast::Access(Access { lhs, rhs, .. }) => lhs
                .to_ts()
                .append(D::text("["))
                .append(rhs.to_ts())
                .append(D::text("]"))
                .group(),
            Ast::TypeLiteral(value) => value.to_ts(),
            Ast::ApplyGeneric(value) => value.to_ts(),
            Ast::Tuple(Tuple { items, .. }) => {
                let sep = D::text(",").append(D::space());

                let items = D::intersperse(items.iter().map(|item| item.to_ts()), sep);

                D::text("[").append(items).append(D::text("]"))
            }
            Ast::Array(node) => {
                let doc = if node.is_set_op() {
                    parens(node.to_ts())
                } else {
                    node.to_ts()
                };

                doc.append(D::text("[]"))
            }
            Ast::NeverKeyword(_) => D::text("never"),
            Ast::AnyKeyword(_) => D::text("any"),
            Ast::UnknownKeyword(_) => D::text("unknown"),
            Ast::TrueKeyword(_) => D::text("true"),
            Ast::FalseKeyword(_) => D::text("false"),
            Ast::Infer(value) => D::text("infer").append(D::space()).append(value.to_ts()),

            Ast::Builtin(Builtin { name, argument, .. }) => {
                name.to_ts().append(" ").append(argument.to_ts())
            }

            Ast::ExtendsExpr(ExtendsExpr {
                lhs,
                rhs,
                then_branch: then,
                else_branch: els,
                ..
            }) => {
                let condition_doc = lhs
                    .to_ts()
                    .append(D::space())
                    .append("extends")
                    .append(D::space())
                    .append(rhs.to_ts());

                let then_doc = D::line()
                    .append("?")
                    .append(D::space())
                    .append(then.to_ts())
                    .nest(4);

                let else_doc = D::line()
                    .append(":")
                    .append(D::space())
                    .append(els.to_ts())
                    .nest(4);

                condition_doc.append(then_doc).append(else_doc)
            }
            Ast::ExtendsInfixOp(ExtendsInfixOp {
                lhs,
                op: InfixOp::Extends,
                rhs,
                ..
            }) => lhs
                .to_ts()
                .append(D::space())
                .append("extends")
                .append(D::space())
                .append(rhs.to_ts())
                .group(),
            Ast::Statement(stmnt) => stmnt.to_ts().append(D::text(";")),
            Ast::MappedType(MappedType {
                index: key,
                iterable,
                remapped_as,
                readonly_mod,
                optional_mod,
                body,
                ..
            }) => {
                let remapped_as_doc = match remapped_as {
                    Some(remapped_as) => D::space()
                        .append("as")
                        .append(D::space())
                        .append(remapped_as.to_ts()),
                    None => D::nil(),
                };

                let lhs_doc = D::nil()
                    .append(key)
                    .append(D::space())
                    .append("in")
                    .append(D::space())
                    .append(iterable.to_ts())
                    .append(remapped_as_doc)
                    .group();

                let rhs_doc = body.to_ts();

                let rhs_doc = D::line().append(rhs_doc).nest(4).group();

                let readonly_doc = match readonly_mod {
                    Some(MappingModifier::Add) => D::text("readonly"),
                    Some(MappingModifier::Remove) => D::text("-readonly"),
                    None => D::nil(),
                };

                let optional_doc = match optional_mod {
                    Some(MappingModifier::Add) => D::text("?"),
                    Some(MappingModifier::Remove) => D::text("-?"),
                    None => D::nil(),
                };

                let inner_doc = D::line()
                    .append(readonly_doc)
                    .append("[")
                    .append(lhs_doc)
                    .append("]")
                    .append(optional_doc)
                    .append(":")
                    .append(rhs_doc)
                    .append(D::line())
                    .nest(4)
                    .group();

                D::nil().append("{").append(inner_doc).append("}").group()
            }
            Ast::LetExpr(..) => {
                unreachable!("LetExpr should be desugared before this point")
            }
            Ast::ImportStatement(ImportStatement {
                import_clause,
                module,
                ..
            }) => {
                let import_clause = import_clause.to_ts();

                D::text("import type")
                    .append(D::space())
                    .append(import_clause)
                    .append(D::space())
                    .append("from")
                    .append(D::space())
                    .append(string_literal(module))
            }
            Ast::Path(Path { segments, .. }) => {
                let sep = D::text(".");

                let segments = D::intersperse(segments.iter().map(|seg| seg.to_ts()), sep);

                segments
            }
            Ast::Interface(value) => {
                return value.to_ts();
            }
            Ast::UnitTest(_) => D::nil(),
            Ast::MacroCall(_) => unreachable!("MacroCall should be desugared before this point"),
            Ast::UnionType(UnionType { types, .. }) => {
                let sep = D::line().append(D::text("|")).append(D::space());
                D::intersperse(
                    types.iter().map(|t| match t {
                        Ast::IntersectionType(IntersectionType { .. }) => {
                            surround(t.to_ts(), "(", ")")
                        }
                        _ => t.to_ts(),
                    }),
                    sep,
                )
                .group()
            }
            Ast::IntersectionType(IntersectionType { types, .. }) => {
                let sep = D::line().append(D::text("&")).append(D::space());
                D::intersperse(types.iter().map(|t| t.to_ts()), sep).group()
            }
            Ast::NoOp(_) => D::nil(),
            node @ (Ast::ExtendsPrefixOp(ExtendsPrefixOp { .. })
            | Ast::MatchExpr(match_expr::MatchExpr { .. })
            | Ast::CondExpr(cond_expr::CondExpr { .. })
            | Ast::ExtendsInfixOp(ExtendsInfixOp { .. })) => {
                unreachable!("Ast should be desugared before this point {:#?}", node)
            }
            Ast::FunctionType(ty) => ty.to_ts(),
        }
    }
}

impl From<ExtendsExpr> for Ast {
    fn from(v: ExtendsExpr) -> Self {
        Ast::ExtendsExpr(v)
    }
}

#[cfg(test)]
mod tests {
    use crate::{parser::Rule, test_support::*};
    use rstest::rstest;
    static TRUE: ExtendsResult = ExtendsResult::True;
    static FALSE: ExtendsResult = ExtendsResult::False;
    static BOTH: ExtendsResult = ExtendsResult::Both;
    static NEVER: ExtendsResult = ExtendsResult::Never;

    use super::*;

    #[rstest]
    // never
    #[case("never", "any", NEVER)]
    #[case("never", "unknown", NEVER)]
    #[case("never", "{}", NEVER)]
    #[case("never", "[]", NEVER)]
    #[case("never", "string", NEVER)]
    #[case("never", "number", NEVER)]
    #[case("never", "object", NEVER)]
    #[case("never", "boolean", NEVER)]
    #[case("never", "Object", NEVER)]
    #[case("never", "1", NEVER)]
    #[case("never", "'string'", NEVER)]
    #[case("never", "true", NEVER)]
    #[case("never", "false", NEVER)]
    #[case("never", "null", NEVER)]
    #[case("never", "undefined", NEVER)]
    #[case("never", "never", NEVER)]
    // any
    #[case("any", "any", TRUE)]
    #[case("any", "unknown", TRUE)]
    #[case("any", "{}", BOTH)]
    #[case("any", "[]", BOTH)]
    #[case("any", "string", BOTH)]
    #[case("any", "number", BOTH)]
    #[case("any", "boolean", BOTH)]
    #[case("any", "object", BOTH)]
    #[case("any", "Object", BOTH)]
    #[case("any", "1", BOTH)]
    #[case("any", "'string'", BOTH)]
    #[case("any", "true", BOTH)]
    #[case("any", "false", BOTH)]
    #[case("any", "null", BOTH)]
    #[case("any", "undefined", BOTH)]
    #[case("any", "never", BOTH)]
    // unknown
    #[case("unknown", "any", TRUE)]
    #[case("unknown", "unknown", TRUE)]
    #[case("unknown", "{}", FALSE)]
    #[case("unknown", "[]", FALSE)]
    #[case("unknown", "string", FALSE)]
    #[case("unknown", "number", FALSE)]
    #[case("unknown", "boolean", FALSE)]
    #[case("unknown", "object", FALSE)]
    #[case("unknown", "Object", FALSE)]
    #[case("unknown", "1", FALSE)]
    #[case("unknown", "'string'", FALSE)]
    #[case("unknown", "true", FALSE)]
    #[case("unknown", "false", FALSE)]
    #[case("unknown", "null", FALSE)]
    #[case("unknown", "undefined", FALSE)]
    #[case("unknown", "never", FALSE)]
    // {}
    #[case("{}", "any", TRUE)]
    #[case("{}", "unknown", TRUE)]
    #[case("{}", "{}", TRUE)]
    #[case("{}", "{ x: string }", FALSE)]
    #[case("{}", "[]", FALSE)]
    #[case("{}", "[{}]", FALSE)]
    #[case("{}", "{}[]", FALSE)]
    #[case("{}", "string", FALSE)]
    #[case("{}", "number", FALSE)]
    #[case("{}", "boolean", FALSE)]
    #[case("{}", "bitint", FALSE)]
    #[case("{}", "symbol", FALSE)]
    #[case("{}", "object", TRUE)]
    #[case("{}", "Object", TRUE)]
    #[case("{}", "Function", FALSE)]
    #[case("{}", "String", FALSE)]
    #[case("{}", "1", FALSE)]
    #[case("{}", "'string'", FALSE)]
    #[case("{}", "`string${var}`", FALSE)]
    #[case("{}", "true", FALSE)]
    #[case("{}", "false", FALSE)]
    #[case("{}", "null", FALSE)]
    #[case("{}", "undefined", FALSE)]
    #[case("{}", "never", FALSE)]
    // string
    #[case("string", "any", TRUE)]
    #[case("string", "unknown", TRUE)]
    #[case("string", "{}", FALSE)]
    #[case("string", "[]", FALSE)]
    #[case("string", "string", TRUE)]
    #[case("string", "number", FALSE)]
    #[case("string", "boolean", FALSE)]
    #[case("string", "Object", TRUE)]
    #[case("string", "Function", FALSE)]
    #[case("string", "String", TRUE)]
    #[case("string", "1", FALSE)]
    #[case("string", "'string'", FALSE)]
    #[case("string", "true", FALSE)]
    #[case("string", "false", FALSE)]
    #[case("string", "null", FALSE)]
    #[case("string", "undefined", FALSE)]
    #[case("string", "never", FALSE)]
    // String
    #[case("String", "any", TRUE)]
    #[case("String", "unknown", TRUE)]
    #[case("String", "{}", FALSE)]
    #[case("String", "[]", FALSE)]
    #[case("String", "string", TRUE)]
    #[case("String", "number", FALSE)]
    #[case("String", "boolean", FALSE)]
    #[case("String", "Object", TRUE)]
    #[case("String", "Function", FALSE)]
    #[case("String", "String", TRUE)]
    #[case("String", "1", FALSE)]
    #[case("String", "'string'", FALSE)]
    #[case("String", "true", FALSE)]
    #[case("String", "false", FALSE)]
    #[case("String", "null", FALSE)]
    #[case("String", "undefined", FALSE)]
    #[case("String", "never", FALSE)]
    #[trace]
    fn is_subtype(#[case] a: &str, #[case] b: &str, #[case] expected: ExtendsResult) {
        assert_eq!(ast!(a).is_subtype(&ast!(b)), expected);
    }
}

#[cfg(test)]
mod simplify_tests {
    use super::*;
    use crate::{
        ast::{
            self,
            Ast::{self, *},
        },
        parser::Rule::expr,
        pest::Parser,
        test_support::parse,
    };
    use lexpr::sexp;
    use pretty_assertions::{assert_eq, assert_ne};
    use rstest::rstest;

    #[rstest]
    #[case(
        "A::B::C::D",
        sexp!((:: (segments (ident . "A") (ident . "B") (ident . "C") (ident . "D"))))
    )]
    fn test_simplify_expr(#[case] input: &str, #[case] expected: lexpr::Value) {
        use crate::parser::{self, Rule};

        let result = parser::NewtypeParser::parse(Rule::expr, input);

        match result {
            Ok(pairs) => {
                let actual = parser::parse_expr(pairs).simplify();

                pretty_assertions::assert_eq!(
                    actual.to_sexp().unwrap().to_string(),
                    expected.to_string()
                );
            }
            Err(err) => {
                panic!("{}", err);
            }
        }
    }

    #[test]
    fn simplify_basic() {
        assert_eq!(
            parse!(expr, "if a <: b then c else d end")
                .simplify()
                .to_sexp()
                .unwrap()
                .to_string(),
            lexpr::sexp!(
                (#"extends-expr"
                    (lhs ident . "a")
                    (rhs ident . "b")
                    (#"then-branch" ident . "c")
                    (#"else-branch" ident . "d")))
            .to_string()
        )
    }

    #[test]
    fn simplify_not() {
        assert_eq!(
            parse!(
                expr,
                r#"
                if a <: b then
                    d
                else
                    c
                end
                "#
            )
            .simplify()
            .to_sexp()
            .unwrap(),
            parse!(
                expr,
                r#"
                if not (a <: b) then
                    c
                else
                    d
                end
                "#
            )
            .simplify()
            .to_sexp()
            .unwrap(),
        )
    }

    #[test]
    fn simplify_and() {
        assert_eq!(
            parse!(
                expr,
                r#"
                if a <: b and c <: d then
                    e
                else
                    f
                end
                "#
            )
            .simplify()
            .to_sexp()
            .unwrap(),
            parse!(
                expr,
                r#"
                if a <: b then
                    if c <: d then
                        e
                    else
                        f
                    end
                else
                    f
                end
                "#
            )
            .simplify()
            .to_sexp()
            .unwrap(),
        )
    }

    #[test]
    fn simplify_or() {
        assert_eq!(
            parse!(
                expr,
                r#"
                if a <: b or c <: d
                then e
                else f
                end
                "#
            )
            .simplify()
            .to_sexp()
            .unwrap(),
            parse!(
                expr,
                r#"
                if a <: b then e
                else
                    if c <: d then e
                    else f
                    end
                end
                "#
            )
            .simplify()
            .to_sexp()
            .unwrap(),
        )
    }

    #[test]
    fn simplify_match_expr() {
        assert_eq!(
            parse!(
                expr,
                r#"
                match A do
                    number -> 1,
                    string -> 2,
                    else -> 3
                end
                "#
            )
            .simplify()
            .to_sexp()
            .unwrap()
            .to_string(),
            parse!(
                expr,
                r#"
                if A <: number then 1
                else
                    if A <: string then 2
                    else 3
                    end
                end
                "#
            )
            .simplify()
            .to_sexp()
            .unwrap()
            .to_string()
        )
    }
}

#[derive(Derivative, Clone, Eq, Serialize)]
#[derivative(PartialEq)]
#[derivative(Debug)]
#[serde(rename_all = "kebab-case")]
pub enum ImportClause {
    Named(Vec<ImportSpecifier>),
    Namespace { alias: Ident },
}

impl typescript::Pretty for ImportClause {
    fn to_ts(&self) -> D<()> {
        match self {
            ImportClause::Named(specifiers) => {
                let sep = D::text(",").append(D::line());

                let specifiers =
                    D::intersperse(specifiers.iter().map(typescript::Pretty::to_ts), sep);

                D::text("{")
                    .append(
                        D::nil()
                            .append(D::line())
                            .append(specifiers)
                            .append(D::line())
                            .nest(4),
                    )
                    .append(D::text("}"))
                    .group()
            }
            ImportClause::Namespace { alias } => D::text("*")
                .append(D::space())
                .append("as")
                .append(D::space())
                .append(alias.to_ts()),
        }
    }
}

#[ast_node]
pub struct ImportSpecifier {
    pub module_export_name: Ident,
    pub alias: Option<Ident>,
}

impl typescript::Pretty for ImportSpecifier {
    fn to_ts(&self) -> D<()> {
        let alias_doc = match &self.alias {
            Some(alias) => D::space()
                .append("as")
                .append(D::space())
                .append(alias.to_ts()),

            None => D::nil(),
        };

        self.module_export_name.to_ts().append(alias_doc)
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum BuiltinKeyword {
    Keyof,
}

impl typescript::Pretty for BuiltinKeyword {
    fn to_ts(&self) -> D<()> {
        match self {
            BuiltinKeyword::Keyof => D::text("keyof"),
        }
    }
}

#[derive(Derivative, Clone, Eq, Serialize)]
#[derivative(PartialEq)]
#[derivative(Debug)]
#[serde(rename_all = "kebab-case")]
pub enum PrimitiveType {
    Boolean,
    Number,
    String,
    Object,
    Symbol,
    BigInt,
    Void,
    Undefined,
    Null,
}

impl Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimitiveType::Boolean => write!(f, "boolean"),
            PrimitiveType::Number => write!(f, "number"),
            PrimitiveType::String => write!(f, "string"),
            PrimitiveType::Object => write!(f, "object"),
            PrimitiveType::Symbol => write!(f, "symbol"),
            PrimitiveType::BigInt => write!(f, "bigint"),
            PrimitiveType::Void => write!(f, "void"),
            PrimitiveType::Undefined => write!(f, "undefined"),
            PrimitiveType::Null => write!(f, "null"),
        }
    }
}

#[derive(Derivative, Clone, Eq, Serialize)]
#[derivative(PartialEq)]
#[derivative(Debug)]
#[serde(rename_all = "kebab-case")]
pub enum PrefixOp {
    Infer,
    Not,
}

impl PrefixOp {
    /// Returns `true` if the prefix op is [`Not`].
    ///
    /// [`Not`]: PrefixOp::Not
    #[must_use]
    pub fn is_not(&self) -> bool {
        matches!(self, Self::Not)
    }
}

#[derive(Derivative, Clone, Eq, Serialize)]
#[derivative(PartialEq)]
#[derivative(Debug)]
#[serde(rename_all = "kebab-case")]
pub enum InfixOp {
    Extends,
    NotExtends,
    Equals,
    NotEquals,
    StrictEquals,
    StrictNotEquals,
    And,
    Or,
}

#[derive(Derivative, Clone, Eq, Serialize)]
#[derivative(PartialEq)]
#[derivative(Debug)]
#[serde(rename_all = "kebab-case")]
pub enum MappingModifier {
    Add,
    Remove,
}

#[derive(Derivative, Clone, Eq, Serialize)]
#[derivative(PartialEq)]
#[derivative(Debug)]
#[serde(rename_all = "kebab-case")]
pub enum ObjectPropertyKey {
    Index(PropertyKeyIndex),
    Key(String),
    Computed(Ident),
}

impl ObjectPropertyKey {
    fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        match self {
            ObjectPropertyKey::Index(index) => ObjectPropertyKey::Index(index.map(f)),
            ObjectPropertyKey::Computed(id) => ObjectPropertyKey::Computed(id.clone()),
            _ => self.clone(),
        }
    }
}

impl typescript::Pretty for ObjectPropertyKey {
    fn to_ts(&self) -> D<()> {
        match self {
            ObjectPropertyKey::Index(index) => surround(index.to_ts(), "[", "]").group(),
            ObjectPropertyKey::Key(key) => D::text(key.clone()),
            ObjectPropertyKey::Computed(id) => surround(id.to_ts(), "[", "]").group(),
        }
    }
}

#[ast_node]
pub struct PropertyKeyIndex {
    pub key: String,
    pub iterable: Ast,
    pub remapped_as: Option<Ast>,
}

impl PropertyKeyIndex {
    fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        Self {
            iterable: f(&self.iterable),
            remapped_as: self.remapped_as.as_ref().map(f),
            ..self.clone()
        }
    }
}

impl typescript::Pretty for PropertyKeyIndex {
    fn to_ts(&self) -> D<()> {
        let PropertyKeyIndex {
            key,
            iterable,
            remapped_as,
            ..
        } = self;

        let remapped_as = match remapped_as {
            Some(remapped_as) => D::space()
                .append("as")
                .append(D::space())
                .append(remapped_as.to_ts()),
            None => D::nil(),
        };

        D::nil()
            .append(key)
            .append(D::space())
            .append("in")
            .append(D::space())
            .append(iterable.to_ts())
            .append(remapped_as)
    }
}
#[ast_node]
pub struct ObjectProperty {
    pub readonly: bool,
    pub optional: bool,
    pub key: ObjectPropertyKey,
    pub value: Ast,
}

impl ObjectProperty {
    fn map<F>(self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        Self {
            value: f(&self.value),
            key: self.key.map(f),
            ..self
        }
    }
}

impl typescript::Pretty for ObjectProperty {
    fn to_ts(&self) -> D<()> {
        let readonly = if self.readonly {
            D::text("readonly").append(D::space())
        } else {
            D::nil()
        };

        let optional = if self.optional {
            D::text("?")
        } else {
            D::nil()
        };

        let doc = D::nil();

        doc.append(readonly)
            .append(self.key.to_ts())
            .append(optional)
            .append(D::text(":"))
            .append(D::space())
            .append(self.value.to_ts())
    }
}

#[ast_node(transparent)]
pub struct Ident {
    pub name: String,
}

impl Ident {
    fn pretty(&self) -> D<()> {
        D::text(&self.name)
    }
}

impl typescript::Pretty for Ident {
    fn to_ts(&self) -> D<()> {
        D::text(self.name.clone())
    }
}

#[ast_node]
pub struct TypeParameter {
    pub name: String,
    pub constraint: Option<Ast>,
    pub default: Option<Ast>,
    pub rest: bool,
}

impl TypeParameter {
    pub fn map<F>(&self, f: F) -> Self
    where
        F: Fn(&Ast) -> Ast,
    {
        Self {
            constraint: self.constraint.as_ref().map(&f),
            default: self.default.as_ref().map(&f),
            ..self.clone()
        }
    }

    pub fn new(
        name: String,
        constraint: Option<Ast>,
        default: Option<Ast>,
        rest: bool,
        span: Span,
    ) -> Self {
        Self {
            name,
            constraint,
            default,
            rest,
            span,
        }
    }
}

impl typescript::Pretty for TypeParameter {
    fn to_ts(&self) -> D<()> {
        let rest = if self.rest { D::text("...") } else { D::nil() };

        let constraint = match &self.constraint {
            Some(constraint) => D::space()
                .append("extends")
                .append(D::space())
                .append(constraint.to_ts()),
            None => D::nil(),
        };

        let default_value = match &self.default {
            Some(value) => D::space()
                .append("=")
                .append(D::space())
                .append(value.to_ts()),
            None => D::nil(),
        };

        D::nil()
            .append(rest)
            .append(self.name.clone())
            .append(constraint)
            .append(default_value)
    }
}

pub(crate) mod if_expr;

pub(crate) mod match_expr;

pub(crate) mod cond_expr;

pub(crate) mod let_expr;
