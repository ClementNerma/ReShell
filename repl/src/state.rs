use std::sync::{Arc, RwLock};

use once_cell::sync::Lazy;
use reshell_runtime::scoping::Context;

pub static RUNTIME_CONTEXT: Lazy<Arc<RwLock<Context>>> =
    Lazy::new(|| Arc::new(RwLock::new(Context::new())));

pub fn with_writable_rt_ctx(func: impl FnOnce(&mut Context)) {
    func(&mut RUNTIME_CONTEXT.write().unwrap())
}
