// TODO: try to find a way to get rid of threads entirely?
// And move Ctrl+C logic elsewhere

use std::{
    sync::Mutex,
    time::{Duration, Instant},
};

static UNTREATED_CTRL_C: Mutex<UntreatedCtrlCStatus> =
    Mutex::new(UntreatedCtrlCStatus::HandlerNotSetup);

#[derive(PartialEq, Eq)]
enum UntreatedCtrlCStatus {
    HandlerNotSetup,
    NoUntreated,
    PendingUntreated,
}

pub fn setup_ctrl_c_handler() -> Result<(), ctrlc::Error> {
    let mut status = UNTREATED_CTRL_C.lock().unwrap();

    if *status != UntreatedCtrlCStatus::HandlerNotSetup {
        return Ok(());
    }

    *status = UntreatedCtrlCStatus::NoUntreated;

    ctrlc::try_set_handler(|| {
        *UNTREATED_CTRL_C.lock().unwrap() = UntreatedCtrlCStatus::PendingUntreated
    })
}

pub fn take_pending_ctrl_c_request() -> bool {
    let mut untreated = UNTREATED_CTRL_C.lock().unwrap();

    match &*untreated {
        UntreatedCtrlCStatus::HandlerNotSetup => panic!("Ctrl+C handler was not set up yet!"),
        UntreatedCtrlCStatus::NoUntreated => false,
        UntreatedCtrlCStatus::PendingUntreated => {
            *untreated = UntreatedCtrlCStatus::NoUntreated;
            true
        }
    }
}

pub fn yield_for_at_least(at_least: Duration) {
    let started_waiting = Instant::now();

    std::thread::yield_now();

    let yielded_for = started_waiting.elapsed();

    if yielded_for < at_least {
        std::thread::sleep(at_least - yielded_for)
    }
}
