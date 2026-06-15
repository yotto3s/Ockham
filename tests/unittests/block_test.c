#include "arena.h"
#include "context.h"
#include "ir.h"
#include "ockham/ockham.h"
#include "unity.h"

static OkmContext ctx;
static OkmFunction* func;

void setUp(void) {
    okm_arena_init(&ctx.arena);
    func = okm_emit_function(&ctx, "test_func", OKM_TY_I32);
}

void tearDown(void) { okm_arena_destroy(&ctx.arena); }

void test_EmitBlock_FirstBlockId(void) {
    OkmBlock* block = okm_emit_block(&ctx, func);
    TEST_ASSERT_EQUAL_UINT32(0u, block->id);
}

void test_EmitBlock_FirstBlockInitialState(void) {
    OkmBlock* block = okm_emit_block(&ctx, func);
    TEST_ASSERT_NULL(block->params);
    TEST_ASSERT_EQUAL_UINT32(0u, block->param_count);
    TEST_ASSERT_NULL(block->instr_head);
    TEST_ASSERT_NULL(block->instr_tail);
    TEST_ASSERT_NULL(block->prev);
    TEST_ASSERT_NULL(block->next);
}

void test_EmitBlock_UpdatesFunctionHeadTail(void) {
    OkmBlock* block = okm_emit_block(&ctx, func);
    TEST_ASSERT_EQUAL_PTR(block, func->block_head);
    TEST_ASSERT_EQUAL_PTR(block, func->block_tail);
}

void test_EmitBlock_SecondBlockId(void) {
    okm_emit_block(&ctx, func);
    OkmBlock* b2 = okm_emit_block(&ctx, func);
    TEST_ASSERT_EQUAL_UINT32(1u, b2->id);
}

void test_EmitBlock_LinkedList(void) {
    OkmBlock* b1 = okm_emit_block(&ctx, func);
    OkmBlock* b2 = okm_emit_block(&ctx, func);

    /* b1 -> b2 */
    TEST_ASSERT_EQUAL_PTR(b2, b1->next);
    TEST_ASSERT_NULL(b1->prev);

    /* b2 <- b1 */
    TEST_ASSERT_EQUAL_PTR(b1, b2->prev);
    TEST_ASSERT_NULL(b2->next);

    /* function head/tail */
    TEST_ASSERT_EQUAL_PTR(b1, func->block_head);
    TEST_ASSERT_EQUAL_PTR(b2, func->block_tail);
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_EmitBlock_FirstBlockId);
    RUN_TEST(test_EmitBlock_FirstBlockInitialState);
    RUN_TEST(test_EmitBlock_UpdatesFunctionHeadTail);
    RUN_TEST(test_EmitBlock_SecondBlockId);
    RUN_TEST(test_EmitBlock_LinkedList);
    return UNITY_END();
}
