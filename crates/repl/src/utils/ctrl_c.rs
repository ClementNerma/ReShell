use std::sync::Mutex;

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
