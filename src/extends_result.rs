#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExtendsResult {
    True,
    False,
    Never,
    Both,
}

impl ExtendsResult {
    /// Conjunction used to fold the component checks of a structural relation
    /// (every tuple element, every required property, every member of an
    /// intersection target, …). All components must hold.
    ///
    /// `Never` is treated as satisfied: a `never`-typed component is assignable
    /// to anything (`never` is the bottom type), so a `never` element does not
    /// make the surrounding composite unassignable.
    pub fn and(self, other: Self) -> Self {
        use ExtendsResult::*;
        match (self, other) {
            (False, _) | (_, False) => False,
            (Both, _) | (_, Both) => Both,
            // (True | Never, True | Never)
            _ => True,
        }
    }

    /// Disjunction used to fold relations where *some* component must hold
    /// (assignability to a union target, assignability *from* an intersection
    /// source). Indeterminate (`Both`) wins over a definite `False`.
    pub fn or(self, other: Self) -> Self {
        use ExtendsResult::*;
        match (self, other) {
            (True, _) | (_, True) => True,
            (Both, _) | (_, Both) => Both,
            _ => False,
        }
    }
}

impl From<bool> for ExtendsResult {
    fn from(value: bool) -> Self {
        if value {
            ExtendsResult::True
        } else {
            ExtendsResult::False
        }
    }
}
