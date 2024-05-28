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
        len: OptionalArg<ExactIntType<u64>> = Arg::long_and_short_flag("len", 'l'),
        template: OptionalArg<StringType> = Arg::long_and_short_flag("template", 's'),
        message: OptionalArg<StringType> = Arg::long_and_short_flag("message", 'm'),
        prefix: OptionalArg<StringType> = Arg::long_and_short_flag("prefix", 'p'),
        keep_on_finish: PresenceFlag = Arg::long_and_short_flag("keep-on-finish", 'k')
    )

    -> None // TODO: new type handler for custom
);

#[derive(Debug, Clone)]
pub(super) struct ProgressBarValue {
    pub inner: ProgressBar,
}

impl CustomValueType for ProgressBarValue {
    fn typename(&self) -> &'static str {
        "ProgressBar"
    }

    fn typename_static() -> &'static str
    where
        Self: Sized,
    {
        "ProgressBar"
    }
}

impl PrettyPrintable for ProgressBarValue {
    fn generate_pretty_data(&self, _: &Context) -> PrettyPrintablePiece {
        PrettyPrintablePiece::colored_atomic("<progress bar>", Color::BrightBlack)
    }
}

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
            let mut pb = match len {
                Some(len) => ProgressBar::new(len),
                None => ProgressBar::new_spinner(),
            };

            if let Some(template) = template {
                let template = ProgressStyle::with_template(&template).map_err(|err| {
                    ctx.error(
                        template_at.unwrap(),
                        format!("invalid template provided: {err}"),
                    )
                })?;

                pb = pb.with_style(template);
            }

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
