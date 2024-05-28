//!
//! Ctrl+C handling
//!
//! Allows to check periodically if Ctrl+C was pressed
//!

use std::sync::atomic::{AtomicBool, Ordering};

static UNTREATED_CTRL_C: AtomicBool = AtomicBool::new(false);

pub fn setup_ctrl_c_handler() -> Result<(), ctrlc::Error> {
    ctrlc::try_set_handler(|| {
        UNTREATED_CTRL_C.store(true, Ordering::Relaxed);
    })
}

pub fn take_pending_ctrl_c_request() -> bool {
    UNTREATED_CTRL_C.swap(false, Ordering::Relaxed)
}
