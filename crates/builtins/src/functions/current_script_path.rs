use std::fs;

use parsy::FileId;
use reshell_parser::files::SourceFileLocation;

crate::define_internal_fn!(
    "currentScriptPath",

    ()

    -> Some(NullableType::<StringType>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|at, Args {}, _, ctx| {
        let source_file_id = match at {
            RuntimeCodeRange::Internal(_) => None,
            RuntimeCodeRange::Parsed(range) => match range.start.file_id {
                FileId::None | FileId::Internal | FileId::Custom(_) => None,
                FileId::SourceFile(id) => Some(id),
            },
        };

        let Some(source_file_id) = source_file_id else {
            return Ok(Some(RuntimeValue::Null));
        };

        let source_file = ctx.files_map().get_file(source_file_id).unwrap();

        let path = match source_file.location {
            SourceFileLocation::CustomName(_) => return Ok(Some(RuntimeValue::Null)),

            SourceFileLocation::RealFile(path) => {
                if path.is_absolute() {
                    path
                } else {
                    fs::canonicalize(&path).map_err(|err| {
                        ctx.throw(
                            at,
                            format!("failed to canonicalize path '{}': {err}", path.display()),
                        )
                    })?
                }
            }
        };

        let Some(path) = path.to_str() else {
            return Err(ctx.throw(
                at,
                format!(
                    "current script path contains invalid UTF-8 characters: {}",
                    path.display()
                ),
            ));
        };

        Ok(Some(RuntimeValue::String(path.to_owned())))
    })
}
