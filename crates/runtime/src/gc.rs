use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

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
