use std::{
    cell::{Ref, RefCell, RefMut},
    ops::Deref,
    rc::Rc,
};

use colored::Colorize;
use reshell_parser::ast::RuntimeCodeRange;

use crate::{context::Context, display::dbg_loc, errors::ExecResult};

// Garbage-collectable cell
#[derive(Debug, Clone)]
pub struct GcCell<T> {
    value: Rc<RefCell<T>>,
    read_lock: Rc<RefCell<Option<RuntimeCodeRange>>>,
}

impl<T> GcCell<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: Rc::new(RefCell::new(value)),
            read_lock: Rc::new(RefCell::new(None)),
        }
    }

    pub fn with_ref<U>(&self, map: impl FnOnce(Ref<T>) -> U) -> U {
        map(self.value.borrow())
    }

    pub fn read(&self, at: impl Into<RuntimeCodeRange>) -> GcRef<T> {
        GcRef::new(self.value.borrow(), Rc::clone(&self.read_lock), at.into())
    }

    pub fn write(&self, at: impl Into<RuntimeCodeRange>, ctx: &Context) -> ExecResult<RefMut<T>> {
        self.value.try_borrow_mut().map_err(|_| {
            let borrowed_at = self
                .read_lock
                .borrow()
                .expect("internal error: read lock is not available in GC cell");

            ctx.error(
                at.into(),
                format!(
                    "Failed to write as parent value is borrowed from {}",
                    dbg_loc(borrowed_at, ctx.files_map()).bright_magenta()
                ),
            )
        })
    }
}

pub struct GcRef<'a, T> {
    inner: Ref<'a, T>,
    read_lock: Rc<RefCell<Option<RuntimeCodeRange>>>,
    is_read_lock_initiator: bool,
}

impl<'a, T> GcRef<'a, T> {
    fn new(
        inner: Ref<'a, T>,
        read_lock: Rc<RefCell<Option<RuntimeCodeRange>>>,
        at: RuntimeCodeRange,
    ) -> Self {
        let mut read_lock_mut = read_lock.borrow_mut();

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

impl<'a, T> Drop for GcRef<'a, T> {
    fn drop(&mut self) {
        if self.is_read_lock_initiator {
            *self.read_lock.borrow_mut() = None;
        }
    }
}

impl<'a, T> Deref for GcRef<'a, T> {
    type Target = Ref<'a, T>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

// Garbage-collectable read-only cell
#[derive(Debug)]
pub struct GcReadOnlyCell<T> {
    value: Rc<T>,
}

impl<T> GcReadOnlyCell<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: Rc::new(value),
        }
    }
}

impl<T> Deref for GcReadOnlyCell<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

// NOTE: manual implementation is required due to a limitation of the Rust compiler
impl<T> Clone for GcReadOnlyCell<T> {
    fn clone(&self) -> Self {
        Self {
            value: Rc::clone(&self.value),
        }
    }
}
