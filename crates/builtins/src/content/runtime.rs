use std::collections::BTreeMap;

use indexmap::IndexSet;
use reshell_runtime::size::ComputableSize;

use crate::{content::human_size::human_size, define_internal_fn};

define_internal_fn!(
    "__runtime",

    (
        print_stats: PresenceFlag = Arg::long_flag("print-stats"),
        inspect_bin_path_cache: PresenceFlag = Arg::long_flag("inspect-binaries-path-cache"),
        clear_bin_path_cache: PresenceFlag = Arg::long_flag("clear-binaries-path-cache")
    )

    -> None
);

fn run() -> Runner {
    Runner::new(
        |_,
         Args {
             print_stats,
             inspect_bin_path_cache,
             clear_bin_path_cache,
         },
         _,
         ctx| {
            let mut out = vec!["=============== Runtime stats ===============".to_string()];

            if print_stats {
                out.push(format!(
                    "=> Total context memory usage : {}",
                    human_size(ctx.compute_total_size().try_into().unwrap(), None)
                ));

                out.push(format!(
                    "=> Native library memory usage: {}",
                    human_size(
                        ctx.native_lib_scope_content()
                            .compute_total_size()
                            .try_into()
                            .unwrap(),
                        None
                    )
                ));

                out.push(format!(
                    "=> Current scope ID           : {}",
                    ctx.current_scope().id
                ));

                out.push(format!(
                    "=> Parent scopes              : {}",
                    parent_scopes_to_str(&ctx.current_scope().parent_scopes)
                ));
            }

            if inspect_bin_path_cache {
                out.push("=> Binaries cache:".to_owned());

                let entries = ctx
                    .binaries_resolver()
                    .entries()
                    .iter()
                    .collect::<BTreeMap<_, _>>();

                for (bin, path) in entries {
                    out.push(format!("     |> {bin}: {}", path.display()));
                }
            }

            out.push("=============================================".to_string());

            if clear_bin_path_cache {
                ctx.binaries_resolver().clear();
                out.push("> Binaries path cache was cleared".to_owned());
            }

            println!("{}\n", out.join("\n"));

            Ok(None)
        },
    )
}

fn parent_scopes_to_str(parent_scopes: &IndexSet<u64>) -> String {
    let mut out = Vec::with_capacity(parent_scopes.len());

    for scope_id in parent_scopes.iter().rev() {
        out.push(scope_id.to_string());
    }

    out.join(" -> ")
}
