/// Function composition macro
#[macro_export]
macro_rules! compose {
    ($($rest:expr),+) => {
        |x| { compose!(expand x, $($rest),*) }
    };
    (expand $inner:expr, $function:expr, $($rest:expr),*) => {
        compose!(expand $function($inner), $($rest),*)
    };
    (expand $inner:expr, $function:expr) => {
        $function($inner)
    };
}
