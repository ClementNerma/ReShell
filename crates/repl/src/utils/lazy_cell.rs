///
/// Polyfill until https://github.com/rust-lang/rust/issues/109736 is resolved
///
use std::{ops::Deref, sync::OnceLock};

pub struct LazyCell<T, F = fn() -> T> {
    inner: OnceLock<T>,
    init: F,
}

impl<T, F> LazyCell<T, F> {
    pub const fn new(init: F) -> Self {
        Self {
            inner: OnceLock::new(),
            init,
        }
    }
}

impl<T> Deref for LazyCell<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner.get_or_init(&self.init)
    }
}
