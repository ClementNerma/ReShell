use text_distance::JaroWinkler;

pub fn jaro_winkler_distance(input: &str, candidate: &str) -> f64 {
    JaroWinkler {
        src: input.to_owned(),
        tar: candidate.to_owned(),
        winklerize: true,
    }
    .normalized_distance()
}
