use std::{
    ops::Deref,
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

use reshell_parser::ast::RuntimeCodeRange;
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};

use crate::{
    context::Context, errors::ExecResult, pretty_impl::pretty_printable_runtime_input_range,
};

// Garbage-collectable cell
#[derive(Debug)]
pub struct GcCell<T> {
    value: Arc<RwLock<T>>,
    read_lock: Arc<RwLock<Option<RuntimeCodeRange>>>,
}

impl<T> GcCell<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: Arc::new(RwLock::new(value)),
            read_lock: Arc::new(RwLock::new(None)),
        }
    }

    pub fn read(&'_ self, at: impl Into<RuntimeCodeRange>) -> GcRef<'_, T> {
        GcRef::new(
            self.value.read().unwrap(),
            Arc::clone(&self.read_lock),
            at.into(),
        )
    }

    // TODO: does this actually work now that parallel() exists?
    /// Read the underlying content, with the affirmation that no write may happen on this resource during its read
    pub fn read_promise_no_write(&'_ self) -> RwLockReadGuard<'_, T> {
        self.value.read().unwrap()
    }

    pub fn write(
        &'_ self,
        at: impl Into<RuntimeCodeRange>,
        ctx: &Context,
    ) -> ExecResult<RwLockWriteGuard<'_, T>> {
        self.value.write().map_err(|_| {
            let at = at.into();

            let borrowed_at = self.read_lock.read().unwrap().unwrap_or_else(|| {
                ctx.panic(at, "write lock is not available in garbabe collector cell")
            });

            ctx.hard_error(
                at,
                format!(
                    "Failed to write as parent value is currently borrowed from {}",
                    pretty_printable_runtime_input_range(borrowed_at, ctx.files_map())
                        .display(PrettyPrintOptions::inline())
                ),
            )
        })
    }
}

// NOTE: This is required because of https://github.com/rust-lang/rust/issues/26925
impl<T> Clone for GcCell<T> {
    fn clone(&self) -> Self {
        Self {
            value: Arc::clone(&self.value),
            read_lock: Arc::clone(&self.read_lock),
        }
    }
}

pub struct GcRef<'a, T> {
    inner: RwLockReadGuard<'a, T>,
    read_lock: Arc<RwLock<Option<RuntimeCodeRange>>>,
    is_read_lock_initiator: bool,
}

impl<'a, T> GcRef<'a, T> {
    fn new(
        inner: RwLockReadGuard<'a, T>,
        read_lock: Arc<RwLock<Option<RuntimeCodeRange>>>,
        at: RuntimeCodeRange,
    ) -> Self {
        let mut read_lock_mut = read_lock.write().unwrap();

        let initiator = read_lock_mut.is_none();

        if initiator {
            *read_lock_mut = Some(at);
        }

        drop(read_lock_mut);

        Self {
            inner,
            read_lock,
            is_read_lock_initiator: initiator,
        }
    }
}

impl<T> Drop for GcRef<'_, T> {
    fn drop(&mut self) {
        if self.is_read_lock_initiator {
            *self.read_lock.write().unwrap() = None;
        }
    }
}

impl<'a, T> Deref for GcRef<'a, T> {
    type Target = RwLockReadGuard<'a, T>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
