use std::cmp::{max, min};

pub fn jaro_winkler_distance(input: &str, candidate: &str) -> f64 {
    1.0 - winkler(input, candidate)
}

fn jaro(src: &str, tar: &str) -> f64 {
    let src_len = src.chars().count();
    let tar_len = tar.chars().count();

    if src_len == 0 && tar_len == 0 {
        return 1.0;
    } else if src_len == 0 || tar_len == 0 {
        return 0.0;
    } else if src == tar {
        return 1.0;
    }

    let match_radius = max(src_len, tar_len) / 2 - 1;
    let mut src_matches = vec![false; src_len];
    let mut tar_matches = vec![false; tar_len];

    let mut common_chars: usize = 0;

    for (i, s_char) in src.chars().enumerate() {
        let low = if i > match_radius {
            max(0, i - match_radius) // There was overflow. Don't know why
        } else {
            0
        };
        let high = min(i + match_radius + 1, tar_len);

        for (j, t_char) in tar.chars().enumerate().take(high).skip(low) {
            if t_char == s_char && !tar_matches[j] {
                src_matches[i] = true;
                tar_matches[j] = true;
                common_chars += 1;
                break;
            }
        }
    }

    if common_chars == 0 {
        return 0.0;
    }

    // Transpositions
    let mut k = 0;
    let mut transpositions = 0;

    for (i, _value) in src_matches.iter().enumerate().take(src_len) {
        if !src_matches[i] {
            continue;
        }
        while !tar_matches[k] {
            k += 1;
        }
        if src.as_bytes()[i] != tar.as_bytes()[k] {
            transpositions += 1;
        }
        k += 1;
    }

    ((common_chars as f64 / src_len as f64)
        + (common_chars as f64 / tar_len as f64)
        + ((common_chars - (transpositions / 2)) as f64 / common_chars as f64))
        / 3.0
}

fn winkler(src: &str, tar: &str) -> f64 {
    let jaro_distance = jaro(src, tar);
    let mut prefix_len = 0;
    if jaro_distance > 0.7 {
        for (_i, (s_char, t_char)) in src.chars().zip(tar.chars()).enumerate() {
            if s_char == t_char {
                prefix_len += 1;
            } else {
                break;
            }
        }
        prefix_len = min(4, prefix_len);
        return jaro_distance + (prefix_len as f64 * 0.1 * (1.0 - jaro_distance));
    }

    jaro_distance
}
