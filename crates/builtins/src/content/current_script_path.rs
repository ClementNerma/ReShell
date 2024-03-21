use reshell_parser::files::SourceFileLocation;

crate::define_internal_fn!(
    "currentScriptPath",

    ()

    -> Some(NullableType::<StringType>::direct_underlying_type())
);

fn run() -> Runner {
    Runner::new(|at, Args {}, _, ctx| {
        let Some(source_file_id) = ctx.current_scope().source_file_id() else {
            return Ok(Some(RuntimeValue::Null));
        };

        let source_file = ctx.files_map().get_file(source_file_id).unwrap();

        let path = match source_file.location {
            SourceFileLocation::CustomName(_) => return Ok(Some(RuntimeValue::Null)),

            SourceFileLocation::RealFile(path) => path,
        };

        let Some(path) = path.to_str() else {
            return Err(ctx.error(
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
