use std::sync::{Arc, RwLock};

use once_cell::sync::Lazy;
use reshell_runtime::{conf::RuntimeConf, context::Context};

// TODO: find a way to make this configurable through CLI
pub static RUNTIME_CONTEXT: Lazy<Arc<RwLock<Context>>> =
    Lazy::new(|| Arc::new(RwLock::new(Context::new(RuntimeConf::default()))));
