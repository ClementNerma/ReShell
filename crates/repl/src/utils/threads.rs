use std::time::{Duration, Instant};

pub fn yield_for_at_least(at_least: Duration) {
    let started_waiting = Instant::now();

    std::thread::yield_now();

    let yielded_for = started_waiting.elapsed();

    if yielded_for < at_least {
        std::thread::sleep(at_least - yielded_for)
    }
}
