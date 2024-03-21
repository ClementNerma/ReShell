use std::{cell::RefCell, rc::Rc};

/// A scope ID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(u64);

#[derive(Clone)]
pub struct ScopeIdGenerator {
    counter: Rc<RefCell<u64>>,
}

impl ScopeIdGenerator {
    pub fn new() -> Self {
        Self {
            counter: Rc::new(RefCell::new(1)),
        }
    }

    pub fn gen(&self) -> ScopeId {
        let mut counter = self.counter.borrow_mut();
        *counter += 1;

        ScopeId(*counter)
    }
}

impl Default for ScopeIdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

pub static NATIVE_LIB_SCOPE_ID: ScopeId = ScopeId(0);
