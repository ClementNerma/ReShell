use colored::Color;
use indicatif::{ProgressBar, ProgressFinish, ProgressStyle};
use reshell_runtime::{
    gc::GcReadOnlyCell,
    pretty::{PrettyPrintable, PrettyPrintablePiece},
    values::CustomValueType,
};

crate::define_internal_fn!(
    "progressBar",

    (
        len: OptionalArg<ExactIntType<u64>> = Arg::positional("len"),
        template: OptionalArg<StringType> = Arg::long_and_short_flag("template", 's'),
        message: OptionalArg<StringType> = Arg::long_and_short_flag("message", 'm'),
        prefix: OptionalArg<StringType> = Arg::long_and_short_flag("prefix", 'p'),
        keep_on_finish: PresenceFlag = Arg::long_and_short_flag("keep-on-finish", 'k')
    )

    -> Some(CustomType::<ProgressBarValue>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args {
             len,
             template,
             message,
             prefix,
             keep_on_finish,
         },
         ArgsAt {
             template: template_at,
             ..
         },
         ctx| {
            let style = match template {
                Some(template) => ProgressStyle::with_template(&template).map_err(|err| {
                    ctx.error(
                        template_at.unwrap(),
                        format!("invalid template provided: {err}"),
                    )
                })?,

                None => ProgressStyle::with_template(
                    "[{elapsed_precise}] {bar:40.cyan/blue} {pos:>3}/{len:3} {eta_precise} {msg}",
                )
                .unwrap(),
            };

            let mut pb = match len {
                Some(len) => ProgressBar::new(len),
                None => ProgressBar::new_spinner(),
            };

            pb = pb.with_style(style);

            if let Some(message) = message {
                pb = pb.with_message(message);
            }

            if let Some(prefix) = prefix {
                pb = pb.with_prefix(prefix);
            }

            if keep_on_finish {
                pb = pb.with_finish(ProgressFinish::AndLeave);
            }

            let pb = ProgressBarValue { inner: pb };

            Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
                pb,
            )))))
        },
    )
}

/// Progress bar displayer
#[derive(Debug, Clone)]
pub struct ProgressBarValue {
    pub inner: ProgressBar,
}

impl CustomValueType for ProgressBarValue {
    fn typename(&self) -> &'static str {
        "progressbar"
    }

    fn typename_static() -> &'static str
    where
        Self: Sized,
    {
        "progressbar"
    }
}

impl PrettyPrintable for ProgressBarValue {
    fn generate_pretty_data(&self, _: &Context) -> PrettyPrintablePiece {
        PrettyPrintablePiece::Join(vec![
            PrettyPrintablePiece::colored_atomic("progressBar(", Color::Magenta),
            PrettyPrintablePiece::colored_atomic("<internal>", Color::BrightBlack),
            PrettyPrintablePiece::colored_atomic(")", Color::Magenta),
        ])
    }
}
