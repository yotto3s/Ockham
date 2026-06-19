#include <string.h>

#include "arena.h"
#include "context.h"
#include "ir.h"
#include "ockham/ockham.h"

OkmFunction* okm_new_function(OkmContext* const ctx, const char* const name,
                              const OkmType* return_types,
                              const uint32_t return_type_count) {
    OkmFunction* const func =
        (OkmFunction*)okm_arena_alloc(&ctx->arena, sizeof(OkmFunction));

    func->name = name;
    func->return_types = return_types;
    func->return_type_count = return_type_count;
    if (return_type_count > 0u) {
        func->return_types = (OkmType*)okm_arena_alloc(
            &ctx->arena, sizeof(OkmType) * return_type_count);
        memcpy(func->return_types, return_types,
               sizeof(OkmType) * return_type_count);
    }

    func->params = NULL;
    func->param_count = 0u;

    func->block_head = NULL;
    func->block_tail = NULL;

    func->next_val_id = 0u;
    func->next_block_id = 0u;

    return func;
}
