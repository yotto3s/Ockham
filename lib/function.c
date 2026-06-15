#include "arena.h"
#include "context.h"
#include "ir.h"
#include "ockham/ockham.h"

OkmFunction* okm_emit_function(OkmContext* const ctx, const char* const name,
                               const OkmType return_type) {
    OkmFunction* const func =
        (OkmFunction*)okm_arena_alloc(&ctx->arena, sizeof(OkmFunction));

    func->name = name;
    func->return_type = return_type;

    func->params = NULL;
    func->param_count = 0u;

    func->block_head = NULL;
    func->block_tail = NULL;

    func->next_val_id = 0u;
    func->next_block_id = 0u;

    return func;
}
