use std::cmp;

/// Compute the Levenshtein distance between two strings.
pub fn levenshtein_distance(a: &str, b: &str) -> usize {
    let n = a.chars().count();
    let m = b.chars().count();

    if n == 0 || m == 0 {
        return usize::MAX;
    }

    let mut dcol: Vec<_> = (0..=m).collect();

    for (i, sc) in a.chars().enumerate() {
        let mut current = i;
        dcol[0] = current + 1;

        for (j, tc) in b.chars().enumerate() {
            let next = dcol[j + 1];
            if sc == tc {
                dcol[j + 1] = current;
            } else {
                dcol[j + 1] = cmp::min(current, next);
                dcol[j + 1] = cmp::min(dcol[j + 1], dcol[j]) + 1;
            }
            current = next;
        }
    }

    dcol[m]
}
