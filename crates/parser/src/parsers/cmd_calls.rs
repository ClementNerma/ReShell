use parsy::{
    Parser, ParserConstUtils,
    parsers::helpers::{char, choice, end, filter, just, newline, not, silent_choice, whitespaces},
};

use super::{
    CMD_CAPTURE, COMPUTED_STRING, DELIMITER_CHARS, EXPR, INLINE_CMD_CALL, LAMBDA, LIST_VALUE,
    LITERAL_STRING, LITERAL_VALUE, SPREAD_VALUE, first_ident_char, ident, ms, s, var_name,
};
use crate::{
    ast::{
        CmdArg, CmdCall, CmdCallBase, CmdEnvVar, CmdExternalPath, CmdFlagArg, CmdFlagArgName,
        CmdFlagValueArg, CmdPath, CmdPipe, CmdPipeType, CmdRawString, CmdRawStringPiece,
        CmdRedirects, CmdValueMakingArg, FlagValueSeparator, SingleCmdCall,
    },
    parsers::{SINGLE_CMD_CALL, msnl},
};

pub fn single_cmd_call() -> impl Parser<SingleCmdCall> + Send + Sync {
    let cmd_raw_string = not(just("->")).ignore_then(
        choice::<CmdRawStringPiece, _>((
            // Variables
            var_name.spanned().map(CmdRawStringPiece::Variable),
            // Command output capture
            CMD_CAPTURE
                .static_ref()
                .map(CmdRawStringPiece::CmdCapturedOutput),
            // Literal character suites
            filter(|c| !c.is_whitespace() && !DELIMITER_CHARS.contains(&c))
                .repeated()
                .at_least(1)
                .collect_string()
                .map(CmdRawStringPiece::Literal),
        ))
        .spanned()
        .repeated_into_vec()
        .at_least(1)
        .map(|pieces| CmdRawString { pieces }),
    );

    let raw_cmd_path = filter(|c| !c.is_whitespace() && !DELIMITER_CHARS.contains(&c))
        .repeated()
        .at_least(1)
        .collect_string()
        .validate_or_critical(|cmd_name| {
            // Command name (no slashes)
            !cmd_name.contains(['/', '\\'])
                || (
                    // Absolute command path
                    cmd_name.starts_with(['/', '\\']) ||
                    // Home-dir relative command path
                    cmd_name.starts_with("~/") || cmd_name.starts_with("~\\") ||
                    // Relative command paths
                    cmd_name.starts_with("./") || cmd_name.starts_with("../") || cmd_name.starts_with(".\\") || cmd_name.starts_with("..\\")
                )
            },
            "Relative command paths must start with a dot and a slash (e.g. './path/to/cmd' or '../path/to/cmd')"
        );

    let cmd_path = choice::<CmdPath, _>((
        //
        // External commands
        //
        char('^')
            .ignore_then(
                choice::<CmdExternalPath, _>((
                    // Raw path
                    cmd_raw_string.spanned().map(CmdExternalPath::RawString),
                    // Single-quoted string
                    LITERAL_STRING
                        .static_ref()
                        .spanned()
                        .map(CmdExternalPath::LiteralString),
                    // Double-quoted string
                    COMPUTED_STRING
                        .static_ref()
                        .spanned()
                        .map(CmdExternalPath::ComputedString),
                ))
                .critical("expected a valid command name after the external marker '^'"),
            )
            .map(CmdPath::External),
        //
        // Methods
        //
        char('.')
            .ignore_then(ident.spanned())
            .map(CmdPath::Method)
            .not_followed_by(filter(|c| {
                !c.is_whitespace() && !DELIMITER_CHARS.contains(&c)
            })),
        //
        // Raw command paths
        //
        raw_cmd_path.spanned().map(CmdPath::Raw),
    ));

    let cmd_value_making_arg = choice::<CmdValueMakingArg, _>((
        // Literal values
        LITERAL_VALUE
            .static_ref()
            // Disambiguation: literal values should be followed by either
            // a space, a newline, a delimiter character or the end of the program
            // Otherwise this means we're in a raw argument with remaining symbols
            .followed_by(silent_choice((
                whitespaces().at_least_one(), /* includes newlines */
                filter(|c| DELIMITER_CHARS.contains(&c)),
                end(),
            )))
            .map(CmdValueMakingArg::LiteralValue),
        // Variable
        char('$')
            .ignore_then(ident)
            .spanned()
            .followed_by(silent_choice((
                filter(|c| c.is_whitespace() || DELIMITER_CHARS.contains(&c)),
                end(),
            )))
            .map(CmdValueMakingArg::Variable),
        // Computed strings
        COMPUTED_STRING
            .static_ref()
            .map(CmdValueMakingArg::ComputedString),
        // Lists
        LIST_VALUE.static_ref().map(CmdValueMakingArg::List),
        // Parenthesis-wrapped expressions
        char('(')
            .ignore_then(
                EXPR.static_ref()
                    .spanned()
                    .padded_by(msnl)
                    .critical("expected an expression"),
            )
            .then_ignore(char(')').critical_auto_msg())
            .map(CmdValueMakingArg::ParenExpr),
        // Inline command call
        INLINE_CMD_CALL
            .static_ref()
            .map(CmdValueMakingArg::InlineCmdCall),
        // Lambdas
        LAMBDA.static_ref().spanned().map(CmdValueMakingArg::Lambda),
        // Raw argument (but not flags, which aren't value making arguments)
        cmd_raw_string
            .spanned()
            // Avoid parsing e.g. 'err>' as 'err' and then making the parser fail because of the unexpected '>'
            .not_followed_by(silent_choice((char('<'), char('>'))))
            .map(CmdValueMakingArg::CmdRawString),
    ));

    let cmd_env_var = ident
        .spanned()
        .then_ignore(char('='))
        .then(
            cmd_value_making_arg
                .spanned()
                .critical("expected a value for the provided environment variable"),
        )
        .followed_by(
            s.critical("expected another assignment or the command call after the assignment"),
        )
        .map(|(name, value)| CmdEnvVar { name, value });

    let cmd_flag_name_arg = choice::<CmdFlagArgName, _>((
        just("--")
            .ignore_then(
                first_ident_char
                    .then(filter(|c| c == '_' || c == '-' || c.is_alphanumeric()).repeated())
                    .collect_string(),
            )
            .map(CmdFlagArgName::Long),
        char('-')
            .ignore_then(first_ident_char)
            .map(CmdFlagArgName::Short),
    ))
    .followed_by(silent_choice((
        filter(|c| c.is_whitespace() || DELIMITER_CHARS.contains(&c)),
        char('='),
        end(),
    )));

    let cmd_flag_arg = cmd_flag_name_arg
        .spanned()
        .then(
            choice((
                s
                    // Flags must be excluded, otherwise '-a -b'
                    // would have '-b' considered as the value for '-a'
                    .not_followed_by(char('-'))
                    // Same thing goes for rest arguments
                    .not_followed_by(just("...").then(char('$').or(char('('))))
                    .to(FlagValueSeparator::Space),
                char('=').to(FlagValueSeparator::Equal),
            ))
            .then(cmd_value_making_arg.spanned())
            .map(|(value_sep, value)| CmdFlagValueArg { value, value_sep })
            .or_not(),
        )
        .map(|(name, value)| CmdFlagArg { name, value });

    let cmd_arg = choice::<CmdArg, _>((
        // Flag arguments
        cmd_flag_arg.map(CmdArg::Flag),
        // Spread
        SPREAD_VALUE.static_ref().spanned().map(CmdArg::Spread),
        // Value-making
        cmd_value_making_arg.spanned().map(CmdArg::ValueMaking),
    ));

    cmd_env_var
        .spanned()
        .then_ignore(s.critical("expected a space after the variable's value"))
        .repeated_into_vec()
        .spanned()
        .then(cmd_path.spanned().followed_by(silent_choice((
            end(),
            filter(|c| {
                c.is_whitespace() || c == ';' || c == '|' || c == '#' || c == ')' || c == '}'
            }),
        ))))
        .then(
            s.ignore_then(
                char('\\')
                    .then(ms)
                    .then(newline())
                    .then(
                        s.critical("expected at least one space for identation after the newline"),
                    )
                    .or_not(),
            )
            .ignore_then(cmd_arg.spanned())
            .repeated_into_vec()
            .spanned(),
        )
        .then(
            s.ignore_then(
                choice::<CmdRedirects, _>((
                    // Redirect both STDOUT and STDERR to a file
                    silent_choice((just("out+err>"), just("err+out>")))
                        .ignore_then(s)
                        .ignore_then(cmd_raw_string.spanned())
                        .map(CmdRedirects::StdoutAndStderrToFile),
                    // Redirect STDOUT to a file and STDERR to another
                    just(">")
                        .ignore_then(s)
                        .ignore_then(cmd_raw_string.spanned())
                        .then_ignore(s)
                        .then_ignore(just("err>"))
                        .then_ignore(s)
                        .then(cmd_raw_string.spanned())
                        .map(|(path_for_stdout, path_for_stderr)| {
                            CmdRedirects::StdoutToFileAndStderrToFile {
                                path_for_stdout,
                                path_for_stderr,
                            }
                        }),
                    just("err>")
                        .ignore_then(s)
                        .ignore_then(cmd_raw_string.spanned())
                        .then_ignore(s)
                        .then_ignore(just(">"))
                        .then_ignore(s)
                        .then(cmd_raw_string.spanned())
                        .map(|(path_for_stderr, path_for_stdout)| {
                            CmdRedirects::StdoutToFileAndStderrToFile {
                                path_for_stdout,
                                path_for_stderr,
                            }
                        }),
                    // Redirect STDERR to a file
                    just("err>")
                        .ignore_then(s)
                        .ignore_then(cmd_raw_string.spanned())
                        .map(CmdRedirects::StderrToFile),
                    // Redirect STDERR to STDOUT
                    just("err>").map(|_| CmdRedirects::StderrToStdout),
                    // Redirect STDOUT to SDTERR
                    just(">err").map(|_| CmdRedirects::StdoutToStderr),
                    // Redirect STDOUT to a file
                    just(">")
                        .ignore_then(s)
                        .ignore_then(cmd_raw_string.spanned())
                        .map(CmdRedirects::StdoutToFile),
                ))
                .spanned(),
            )
            .or_not(),
        )
        .map(|(((env_vars, path), args), redirects)| SingleCmdCall {
            env_vars,
            path,
            args,
            redirects,
        })
}

pub fn cmd_call() -> impl Parser<CmdCall> + Send + Sync {
    let cmd_call_base = choice::<CmdCallBase, _>((
        //
        // Expressions
        //
        EXPR.static_ref()
            // Disambiguation: expressions, after optional spaces, should be followed by either
            // newlines, delimiter characters or the end of the program
            // Otherwise this means we're in a command
            .followed_by(ms.then(silent_choice((
                end(),
                filter(|c| c == '\n' || c == '\r' || DELIMITER_CHARS.contains(&c)),
            ))))
            .map(Box::new)
            .spanned()
            .map(CmdCallBase::Expr),
        //
        // Normal command call
        //
        SINGLE_CMD_CALL
            .static_ref()
            .spanned()
            .map(CmdCallBase::SingleCmdCall),
    ));

    cmd_call_base
        .map(Box::new)
        .then(
            msnl.ignore_then(
                choice::<CmdPipeType, _>((
                    just("^|").to(CmdPipeType::Stderr),
                    char('|').to(CmdPipeType::ValueOrStdout),
                ))
                .spanned(),
            )
            .then_ignore(msnl)
            .then(
                SINGLE_CMD_CALL
                    .static_ref()
                    .spanned()
                    .critical("expected a command call after the pipe"),
            )
            .map(|(pipe_type, cmd)| CmdPipe { cmd, pipe_type })
            .repeated_into_vec(),
        )
        .map(|(base, pipes)| CmdCall { base, pipes })
}
