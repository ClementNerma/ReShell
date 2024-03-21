use std::{cell::RefCell, rc::Rc};

/// A scope ID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstScopeId(u64);

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

    pub fn gen(&self) -> AstScopeId {
        let mut counter = self.counter.borrow_mut();
        *counter += 1;

        AstScopeId(*counter)
    }
}

impl Default for ScopeIdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

pub static NATIVE_LIB_AST_SCOPE_ID: AstScopeId = AstScopeId(0);
