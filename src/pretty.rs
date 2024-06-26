/// Utility functions for pretty printing

use pretty::RcDoc;

pub(crate) fn string_literal(string: &String) -> RcDoc<()> {
    RcDoc::text("\'")
        .append(RcDoc::text(string.replace("\'", "\\\'")))
        .append(RcDoc::text("\'"))
}

pub(crate) fn double_quote(doc: RcDoc<()>) -> RcDoc<()> {
    surround(doc, "\"", "\"")
}

pub(crate) fn parens(doc: RcDoc<()>) -> RcDoc<()> {
    surround(doc, "(", ")")
}

pub(crate) fn single_quote(doc: RcDoc<()>) -> RcDoc<()> {
    surround(doc, "'", "'")
}

pub(crate) fn surround<'a, T>(doc: RcDoc<'a, ()>, left: T, right: T) -> RcDoc<()>
where
    T: Into<std::borrow::Cow<'a, str>>,
{
    RcDoc::text(left).append(doc).append(RcDoc::text(right))
}
