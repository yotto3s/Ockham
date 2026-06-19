#include "arena.h"
#include "context.h"
#include "ir.h"
#include "ockham/ockham.h"
#include "unity.h"

static OkmContext ctx;
static OkmFunction* func;

void setUp(void) {
    okm_arena_init(&ctx.arena);
    const OkmType ret_types[] = {OKM_TY_I32};
    func = okm_new_function(&ctx, "test_func", ret_types, 1u);
}

void tearDown(void) { okm_arena_destroy(&ctx.arena); }

void test_NewBlock_FirstBlockId(void) {
    OkmBlock* block = okm_new_block(&ctx, func);
    TEST_ASSERT_EQUAL_UINT32(0u, block->id);
}

void test_NewBlock_FirstBlockInitialState(void) {
    OkmBlock* block = okm_new_block(&ctx, func);
    TEST_ASSERT_NULL(block->params);
    TEST_ASSERT_EQUAL_UINT32(0u, block->param_count);
    TEST_ASSERT_NULL(block->instr_head);
    TEST_ASSERT_NULL(block->instr_tail);
    TEST_ASSERT_NULL(block->prev);
    TEST_ASSERT_NULL(block->next);
}

void test_NewBlock_UpdatesFunctionHeadTail(void) {
    OkmBlock* block = okm_new_block(&ctx, func);
    TEST_ASSERT_EQUAL_PTR(block, func->block_head);
    TEST_ASSERT_EQUAL_PTR(block, func->block_tail);
}

void test_NewBlock_SecondBlockId(void) {
    okm_new_block(&ctx, func);
    OkmBlock* b2 = okm_new_block(&ctx, func);
    TEST_ASSERT_EQUAL_UINT32(1u, b2->id);
}

void test_NewBlock_HasFunctionPointer(void) {
    OkmBlock* block = okm_new_block(&ctx, func);
    TEST_ASSERT_EQUAL_PTR(func, block->function);
}

void test_NewBlock_LinkedList(void) {
    OkmBlock* b1 = okm_new_block(&ctx, func);
    OkmBlock* b2 = okm_new_block(&ctx, func);

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
    RUN_TEST(test_NewBlock_FirstBlockId);
    RUN_TEST(test_NewBlock_FirstBlockInitialState);
    RUN_TEST(test_NewBlock_UpdatesFunctionHeadTail);
    RUN_TEST(test_NewBlock_SecondBlockId);
    RUN_TEST(test_NewBlock_HasFunctionPointer);
    RUN_TEST(test_NewBlock_LinkedList);
    return UNITY_END();
}
