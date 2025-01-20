use std::{ops::Deref, time::Duration};

use colored::Color;
use indicatif::{ProgressBar, ProgressFinish, ProgressStyle};
use reshell_runtime::{gc::GcReadOnlyCell, values::CustomValueType};
use reshell_shared::pretty::{PrettyPrintable, PrettyPrintablePiece};

crate::define_internal_fn!(
    "progressBar",

    (
        len: OptionalArg<ExactIntType<u64>> = Arg::positional("len"),
        template: OptionalArg<StringType> = Arg::long_and_short_flag("template", 's'),
        message: OptionalArg<StringType> = Arg::long_and_short_flag("message", 'm'),
        prefix: OptionalArg<StringType> = Arg::long_and_short_flag("prefix", 'p'),
        clear_on_finish: PresenceFlag = Arg::long_and_short_flag("clear-on-finish", 'c')
    )

    -> CustomType<ProgressBarValue>
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args {
             len,
             template,
             message,
             prefix,
             clear_on_finish,
         },
         args_at,
         ctx| {
            let style = match template {
                Some(template) => ProgressStyle::with_template(&template).map_err(|err| {
                    ctx.throw(
                        args_at.template.unwrap(),
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

            if !clear_on_finish {
                pb = pb.with_finish(ProgressFinish::AndLeave);
            }

            pb.enable_steady_tick(Duration::from_millis(10));

            Ok(Some(RuntimeValue::Custom(GcReadOnlyCell::new(Box::new(
                ProgressBarValue(pb),
            )))))
        },
    )
}

/// Progress bar displayer
#[derive(Debug, Clone)]
pub struct ProgressBarValue(ProgressBar);

// impl ProgressBarValue {
//     pub fn new(pb: ProgressBar) -> Self {
//         Self(pb)
//     }
// }

impl Deref for ProgressBarValue {
    type Target = ProgressBar;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
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
    type Context = ();

    fn generate_pretty_data(&self, _: &()) -> PrettyPrintablePiece {
        PrettyPrintablePiece::Join(vec![
            PrettyPrintablePiece::colored_atomic("progressBar(", Color::Magenta),
            PrettyPrintablePiece::colored_atomic("<internal>", Color::BrightBlack),
            PrettyPrintablePiece::colored_atomic(")", Color::Magenta),
        ])
    }
}
