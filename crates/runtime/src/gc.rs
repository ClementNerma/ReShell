use std::{
    ops::Deref,
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

use reshell_parser::ast::RuntimeCodeRange;
use reshell_prettify::{PrettyPrintOptions, PrettyPrintable};

use crate::{context::Context, errors::ExecResult};

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

            ctx.error(
                at,
                format!(
                    "Failed to write as parent value is currently borrowed from {}",
                    borrowed_at.display(ctx.files_map(), PrettyPrintOptions::inline())
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

// Garbage-collectable once-assignable cell
#[derive(Debug)]
pub struct GcOnceCell<T> {
    inner: Arc<RwLock<Option<T>>>,
}

impl<T> GcOnceCell<T> {
    pub fn new_uninit() -> Self {
        Self {
            inner: Arc::new(RwLock::new(None)),
        }
    }

    pub fn new_init(value: T) -> Self {
        Self {
            inner: Arc::new(RwLock::new(Some(value))),
        }
    }

    pub fn init(&self, data: T) -> Result<(), GcOnceCellAlreadyInitErr> {
        let mut inner = self.inner.write().unwrap();

        if inner.is_some() {
            return Err(GcOnceCellAlreadyInitErr);
        }

        *inner = Some(data);

        Ok(())
    }

    pub fn get(&'_ self) -> RwLockReadGuard<'_, Option<T>> {
        self.inner.read().unwrap()
    }

    pub fn get_or_init(&self, create: impl FnOnce() -> T) {
        let created = create();
        *self.inner.write().unwrap() = Some(created);
    }
}

// NOTE: This is required because of https://github.com/rust-lang/rust/issues/26925
impl<T> Clone for GcOnceCell<T> {
    fn clone(&self) -> Self {
        Self {
            inner: Arc::clone(&self.inner),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct GcOnceCellAlreadyInitErr;

// Garbage-collectable read-only cell
#[derive(Debug)]
pub struct GcReadOnlyCell<T> {
    value: Arc<T>,
}

impl<T> GcReadOnlyCell<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: Arc::new(value),
        }
    }

    pub fn clone_as_arc(this: &Self) -> Arc<T> {
        Arc::clone(&this.value)
    }
}

impl<T> Deref for GcReadOnlyCell<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

// NOTE: This is required because of https://github.com/rust-lang/rust/issues/26925
impl<T> Clone for GcReadOnlyCell<T> {
    fn clone(&self) -> Self {
        Self {
            value: Arc::clone(&self.value),
        }
    }
}
