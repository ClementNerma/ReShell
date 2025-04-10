use std::sync::{Arc, Mutex};

/// A scope ID
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstScopeId(u64);

#[derive(Clone)]
pub struct ScopeIdGenerator {
    counter: Arc<Mutex<u64>>,
}

impl ScopeIdGenerator {
    pub fn new() -> Self {
        Self {
            counter: Arc::new(Mutex::new(1)),
        }
    }

    pub fn next(&self) -> AstScopeId {
        let mut counter = self.counter.lock().unwrap();
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
