#include "arena.h"
#include "context.h"
#include "ir.h"
#include "ockham/ockham.h"

OkmBlock* okm_new_block(OkmContext* const ctx, OkmFunction* const func) {
    OkmBlock* const block =
        (OkmBlock*)okm_arena_alloc(&ctx->arena, sizeof(OkmBlock));

    block->id = func->next_block_id++;
    block->params = NULL;
    block->param_count = 0u;

    block->function = func;

    block->instr_head = NULL;
    block->instr_tail = NULL;

    block->next = NULL;
    block->prev = func->block_tail;

    if (func->block_tail) {
        func->block_tail->next = block;
    } else {
        func->block_head = block;
    }
    func->block_tail = block;

    return block;
}
