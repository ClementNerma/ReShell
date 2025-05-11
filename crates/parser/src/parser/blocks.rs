use std::sync::atomic::{AtomicU64, Ordering};

use parsy::{
    Parser,
    helpers::{char, end, silent_choice},
    timed::LazilyDefined,
};

use super::instrs::INSTRUCTION;
use crate::{
    NATIVE_LIB_AST_SCOPE_ID,
    ast::{AstScopeId, Block},
    use_basic_parsers,
};

static SCOPE_ID_COUNTER: AtomicU64 = AtomicU64::new(NATIVE_LIB_AST_SCOPE_ID.0 + 1);

pub(crate) fn generate_scope_id() -> AstScopeId {
    AstScopeId(SCOPE_ID_COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub static RAW_BLOCK: LazilyDefined<Block> = LazilyDefined::new(|| {
    use_basic_parsers!(msnl);

    INSTRUCTION
        .static_ref()
        .padded_by(msnl)
        .repeated_into_vec()
        .map(move |instructions| Block {
            scope_id: generate_scope_id(), // TODO: scope_id_gen.next(),
            instructions,
        })
        .followed_by(
            msnl.then(silent_choice((end(), char('}'))))
                .critical("expected an instruction"),
        )
        .erase_type()
});

pub static BLOCK: LazilyDefined<Block> = LazilyDefined::new(|| {
    use_basic_parsers!(msnl);

    char('{')
        .critical_auto_msg()
        .ignore_then(msnl)
        .ignore_then(RAW_BLOCK.static_ref())
        .then_ignore(msnl)
        .then_ignore(char('}').critical_auto_msg())
        .erase_type()
});
