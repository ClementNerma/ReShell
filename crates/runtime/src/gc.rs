use std::{
    ops::Deref,
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

// Garbage-collectable cell
#[derive(Debug, Clone)]
pub struct GcCell<T> {
    value: Arc<RwLock<T>>,
}

impl<T> GcCell<T> {
    pub fn new(value: T) -> Self {
        Self {
            value: Arc::new(RwLock::new(value)),
        }
    }

    pub fn read(&self) -> RwLockReadGuard<'_, T> {
        self.value.read().unwrap()
    }

    pub fn write(&self) -> RwLockWriteGuard<'_, T> {
        self.value.write().unwrap()
    }
}

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
            value: Arc::clone(&self.value),
        }
    }
}
