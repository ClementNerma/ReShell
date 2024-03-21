
# // TODO: > closures
# // TODO: > "set-prompt" fn in first scope that sets it by going directly into 1st scope
# // TODO: > don't clone runtime values! (= fns shouldn't take refs to runtime values but raw values instead)

# // include "./tests/sample.rsh"

$gen_prompt = |prompt_data| {
    let duration_ms = $prompt_data?.last_cmd_status?.duration_ms ?? 0
    let last_exit_code = $prompt_data?.last_cmd_status?.exit_code ?? 0
    let term_width = term_cols()

    # // TODO: use *all* of starship's prompt options!

    set_env STARSHIP_SHELL ""

    return {
        prompt_left: $(starship prompt --cmd-duration $duration_ms --status $last_exit_code --terminal-width $term_width),
        prompt_right: $(starship prompt --right --cmd-duration $duration_ms --status $last_exit_code --terminal-width $term_width),
        prompt_indicator: "",
        prompt_multiline_indicator: $(starship prompt --continuation)
    }
}

set_env STARSHIP_CONFIG ""
