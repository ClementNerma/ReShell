
$gen_prompt = |prompt_data| {
    let duration_ms = $prompt_data?.last_cmd_status?.duration_ms ?? 0
    let last_exit_code = $prompt_data?.last_cmd_status?.exit_code ?? 0
    let term_width = term_cols()

    set_env STARSHIP_SHELL ""

    # NOTE: The beginning space is a fix for Reedline
    return {
        prompt_left: " $(starship prompt --cmd-duration $duration_ms --status $last_exit_code --terminal-width $term_width)",
        prompt_right: $(starship prompt --right --cmd-duration $duration_ms --status $last_exit_code --terminal-width $term_width),
        prompt_indicator: "",
        prompt_multiline_indicator: $(starship prompt --continuation)
    }
}

set_env STARSHIP_CONFIG ""

