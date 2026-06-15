#include "arena.h"
#include "context.h"
#include "ir.h"
#include "ockham/ockham.h"
#include "unity.h"

static OkmContext ctx;

void setUp(void) { okm_arena_init(&ctx.arena); }

void tearDown(void) { okm_arena_destroy(&ctx.arena); }

void test_EmitFunction_Name(void) {
    OkmFunction* func = okm_emit_function(&ctx, "my_func", OKM_TY_I32);
    TEST_ASSERT_EQUAL_STRING("my_func", func->name);
}

void test_EmitFunction_ReturnType(void) {
    OkmFunction* func = okm_emit_function(&ctx, "f", OKM_TY_F64);
    TEST_ASSERT_EQUAL_INT(OKM_TY_F64, func->return_type);
}

void test_EmitFunction_InitialState(void) {
    OkmFunction* func = okm_emit_function(&ctx, "f", OKM_TY_I64);
    TEST_ASSERT_NULL(func->params);
    TEST_ASSERT_EQUAL_UINT32(0u, func->param_count);
    TEST_ASSERT_NULL(func->block_head);
    TEST_ASSERT_NULL(func->block_tail);
    TEST_ASSERT_EQUAL_UINT32(0u, func->next_val_id);
    TEST_ASSERT_EQUAL_UINT32(0u, func->next_block_id);
}

void test_EmitFunction_MultipleIndependent(void) {
    OkmFunction* f1 = okm_emit_function(&ctx, "f1", OKM_TY_I32);
    OkmFunction* f2 = okm_emit_function(&ctx, "f2", OKM_TY_I64);
    TEST_ASSERT_NOT_EQUAL(f1, f2);
    TEST_ASSERT_EQUAL_STRING("f1", f1->name);
    TEST_ASSERT_EQUAL_STRING("f2", f2->name);
    TEST_ASSERT_EQUAL_INT(OKM_TY_I32, f1->return_type);
    TEST_ASSERT_EQUAL_INT(OKM_TY_I64, f2->return_type);
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_EmitFunction_Name);
    RUN_TEST(test_EmitFunction_ReturnType);
    RUN_TEST(test_EmitFunction_InitialState);
    RUN_TEST(test_EmitFunction_MultipleIndependent);
    return UNITY_END();
}
