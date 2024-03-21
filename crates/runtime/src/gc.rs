use std::{
    cell::{Ref, RefCell, RefMut},
    ops::Deref,
    rc::Rc,
};

// Garbage-collectable cell
#[derive(Debug, Clone)]
pub struct GcCell<T> {
    value: Rc<RefCell<T>>,
}

impl<T> GcCell<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: Rc::new(RefCell::new(value)),
        }
    }

    pub fn read(&self) -> Ref<T> {
        self.value.borrow()
    }

    pub fn write(&self) -> RefMut<T> {
        self.value.borrow_mut()
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
