use std::collections::BTreeMap;

use indexmap::IndexSet;

use crate::define_internal_fn;

define_internal_fn!(
    "__runtime",

    (
        print_stats: PresenceFlag = Arg::long_flag("print-stats"),
        inspect_bin_path_cache: PresenceFlag = Arg::long_flag("inspect-binaries-path-cache"),
        clear_bin_path_cache: PresenceFlag = Arg::long_flag("clear-binaries-path-cache")
    )

    -> VoidType
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
            let mut out = vec![];

            if print_stats {
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
