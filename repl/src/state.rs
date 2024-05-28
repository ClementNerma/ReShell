use std::sync::{Arc, RwLock};

use once_cell::sync::Lazy;
use reshell_runtime::context::Context;

pub static RUNTIME_CONTEXT: Lazy<Arc<RwLock<Context>>> =
    Lazy::new(|| Arc::new(RwLock::new(Context::new(None))));
