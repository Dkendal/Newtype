#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExtendsResult {
    True,
    False,
    Never,
    Both,
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
