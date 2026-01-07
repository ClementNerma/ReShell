use rayon::iter::{IntoParallelIterator, IntoParallelRefIterator, ParallelIterator};
use reshell_runtime::errors::ExecResult;

/// Map values of a slice in parallel, while keeping the number of maximum threads at the provided threshold
///
/// `best_ordered` ensures the mapping function is called on items that are as close as possible in the list, starting from
/// the very first item.
///
/// This is in opposition to `rayon`'s `.par_iter()` which segments the slice when mapping.
pub fn parallel_map<'a, T: Send + Sync, U: Send>(
    values: &'a [T],
    mapper: impl Fn(&'a T) -> ExecResult<U> + Send + Sync,
    max_threads: Option<usize>,
    best_ordered: bool,
) -> ExecResult<Vec<U>>
where
    &'a [T]: IntoParallelIterator<Item = &'a T>,
{
    let task = || -> Result<Vec<_>, _> {
        if !best_ordered {
            return values.par_iter().map(mapper).collect();
        }

        let values_len = values.len();
        let values = std::sync::Mutex::new(values.iter().rev().collect::<Vec<_>>());

        (0..values_len)
            .into_par_iter()
            .map(|_| {
                let value = { values.lock().unwrap().pop().unwrap() };
                mapper(value)
            })
            .collect()
    };

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
