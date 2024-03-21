use std::{
    cell::{Ref, RefCell, RefMut},
    ops::Deref,
    rc::Rc,
};

use colored::Colorize;
use reshell_parser::ast::RuntimeCodeRange;

use crate::{context::Context, display::dbg_loc, errors::ExecResult};

// Garbage-collectable cell
#[derive(Debug)]
pub struct GcCell<T: ?Sized> {
    value: Rc<RefCell<T>>,
    read_lock: Rc<RefCell<Option<RuntimeCodeRange>>>,
}

impl<T> GcCell<T> {
    pub fn new(value: T) -> Self {
        Self::new_unsized(Rc::new(RefCell::new(value)))
    }
}

impl<T: ?Sized> GcCell<T> {
    pub fn new_unsized(value: Rc<RefCell<T>>) -> Self {
        Self {
            value,
            read_lock: Rc::new(RefCell::new(None)),
        }
    }

    pub fn read(&self, at: impl Into<RuntimeCodeRange>) -> GcRef<T> {
        GcRef::new(self.value.borrow(), Rc::clone(&self.read_lock), at.into())
    }

    pub fn read_promise_no_write(&self) -> Ref<T> {
        self.value.borrow()
    }

    pub fn write(&self, at: impl Into<RuntimeCodeRange>, ctx: &Context) -> ExecResult<RefMut<T>> {
        self.value.try_borrow_mut().map_err(|_| {
            let at = at.into();

            let borrowed_at = self.read_lock.borrow().unwrap_or_else(|| {
                ctx.panic(at, "write lock is not available in garbabe collector cell")
            });

            ctx.error(
                at,
                format!(
                    "Failed to write as parent value is currently borrowed from {}",
                    dbg_loc(borrowed_at, ctx.files_map()).bright_magenta()
                ),
            )
        })
    }
}

// NOTE: This is required because of https://github.com/rust-lang/rust/issues/26925
impl<T: ?Sized> Clone for GcCell<T> {
    fn clone(&self) -> Self {
        Self {
            value: Rc::clone(&self.value),
            read_lock: Rc::clone(&self.read_lock),
        }
    }
}

pub struct GcRef<'a, T: ?Sized> {
    inner: Ref<'a, T>,
    read_lock: Rc<RefCell<Option<RuntimeCodeRange>>>,
    is_read_lock_initiator: bool,
}

impl<'a, T: ?Sized> GcRef<'a, T> {
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

impl<'a, T: ?Sized> Drop for GcRef<'a, T> {
    fn drop(&mut self) {
        if self.is_read_lock_initiator {
            *self.read_lock.borrow_mut() = None;
        }
    }
}

impl<'a, T: ?Sized> Deref for GcRef<'a, T> {
    type Target = Ref<'a, T>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

// Garbage-collectable once-assignable cell
#[derive(Debug)]
pub struct GcOnceCell<T> {
    inner: Rc<RefCell<Option<T>>>,
}

impl<T> GcOnceCell<T> {
    pub fn new_uninit() -> Self {
        Self {
            inner: Rc::new(RefCell::new(None)),
        }
    }

    pub fn new_init(value: T) -> Self {
        Self {
            inner: Rc::new(RefCell::new(Some(value))),
        }
    }

    pub fn init(&self, data: T) -> Result<(), GcOnceCellAlreadyInitErr> {
        let mut inner = self.inner.borrow_mut();

        if inner.is_some() {
            return Err(GcOnceCellAlreadyInitErr);
        }

        *inner = Some(data);

        Ok(())
    }

    pub fn get(&self) -> Ref<Option<T>> {
        self.inner.borrow()
    }

    pub fn get_or_init(&self, create: impl FnOnce() -> T) {
        let created = create();
        *self.inner.borrow_mut() = Some(created);
    }
}

// NOTE: This is required because of https://github.com/rust-lang/rust/issues/26925
impl<T> Clone for GcOnceCell<T> {
    fn clone(&self) -> Self {
        Self {
            inner: Rc::clone(&self.inner),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct GcOnceCellAlreadyInitErr;

// Garbage-collectable read-only cell
#[derive(Debug)]
pub struct GcReadOnlyCell<T: ?Sized> {
    value: Rc<T>,
}

impl<T> GcReadOnlyCell<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: Rc::new(value),
        }
    }
}

impl<T: ?Sized> GcReadOnlyCell<T> {
    pub fn new_unsized(value: Rc<T>) -> Self {
        Self { value }
    }
}

impl<T: ?Sized> Deref for GcReadOnlyCell<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

// NOTE: This is required because of https://github.com/rust-lang/rust/issues/26925
impl<T: ?Sized> Clone for GcReadOnlyCell<T> {
    fn clone(&self) -> Self {
        Self {
            value: Rc::clone(&self.value),
        }
    }
}

// TODO: CoerceUnsized + DispatchFromDyn for all types here
// In the meantime, these macros can be use
#[macro_export]
macro_rules! gc_cell {
    ($value: expr) => {
        $crate::gc::GcCell::new_unsized(::std::rc::Rc::new(::std::cell::RefCell::new($value)))
    };

    ([readonly] $value: expr) => {
        $crate::gc::GcReadOnlyCell::new_unsized(::std::rc::Rc::new($value))
    };
}
