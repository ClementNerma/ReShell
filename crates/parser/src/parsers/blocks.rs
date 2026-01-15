use std::sync::atomic::{AtomicU64, Ordering};

use parsy::{
    Parser, ParserConstUtils,
    parsers::helpers::{char, end, silent_choice},
};

use super::{INSTRUCTION, RAW_BLOCK, msnl};
use crate::{
    NATIVE_LIB_AST_SCOPE_ID,
    ast::{AstScopeId, Block},
};

static SCOPE_ID_COUNTER: AtomicU64 = AtomicU64::new(NATIVE_LIB_AST_SCOPE_ID.0 + 1);

pub(crate) fn generate_scope_id() -> AstScopeId {
    AstScopeId(SCOPE_ID_COUNTER.fetch_add(1, Ordering::SeqCst))
}

pub fn raw_block() -> impl Parser<Block> + Send + Sync {
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
}

pub fn block() -> impl Parser<Block> + Send + Sync {
    char('{')
        .critical_auto_msg()
        .ignore_then(msnl)
        .ignore_then(RAW_BLOCK.static_ref())
        .then_ignore(msnl)
        .then_ignore(char('}').critical_auto_msg())
}
