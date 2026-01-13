use crate::functions_set;

functions_set! {
    fn string_methods => {
        mod chars;
        mod contains;
        mod ends_with;
        mod find_str;
        mod is_empty;
        mod len;
        mod lines;
        mod pad_end;
        mod pad_start;
        mod parse_float;
        mod parse_int;
        mod parse_json;
        mod parse_toml;
        mod repeat;
        mod replace;
        mod reversed;
        mod split;
        mod starts_with;
        mod substr;
        mod to_lowercase;
        mod to_uppercase;
        mod trim;
        mod trim_end;
        mod trim_start;
    }
}
