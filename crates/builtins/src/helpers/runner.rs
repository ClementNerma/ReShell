pub fn run_parallel<T: Send>(task: impl FnOnce() -> T + Send, max_threads: Option<usize>) -> T {
    match max_threads {
        None => task(),
        Some(max_threads) => {
            let pool = rayon::ThreadPoolBuilder::new()
                .num_threads(max_threads)
                .build()
                .unwrap();

            pool.install(task)
        }
    }
}
