#include "arena.h"
#include "context.h"
#include "ir.h"
#include "ockham/ockham.h"
#include "unity.h"

static OkmContext ctx;

void setUp(void) { okm_arena_init(&ctx.arena); }

void tearDown(void) { okm_arena_destroy(&ctx.arena); }

void test_NewFunction_Name(void) {
    const OkmType ret_types[] = {OKM_TY_I32};
    OkmFunction* func = okm_new_function(&ctx, "my_func", ret_types, 1u);
    TEST_ASSERT_EQUAL_STRING("my_func", func->name);
}

void test_NewFunction_ReturnTypeSingle(void) {
    const OkmType ret_types[] = {OKM_TY_I64};
    OkmFunction* func = okm_new_function(&ctx, "f", ret_types, 1u);
    TEST_ASSERT_EQUAL_UINT32(1u, func->return_type_count);
    TEST_ASSERT_NOT_NULL(func->return_types);
    TEST_ASSERT_EQUAL_INT(OKM_TY_I64, func->return_types[0]);
}

void test_NewFunction_ReturnTypeNone(void) {
    OkmFunction* func = okm_new_function(&ctx, "f", NULL, 0u);
    TEST_ASSERT_EQUAL_UINT32(0u, func->return_type_count);
    TEST_ASSERT_NULL(func->return_types);
}

void test_NewFunction_ReturnTypeMultiple(void) {
    const OkmType ret_types[] = {OKM_TY_I8, OKM_TY_I16, OKM_TY_I32, OKM_TY_I64};
    OkmFunction* func = okm_new_function(&ctx, "f", ret_types, 4u);
    TEST_ASSERT_EQUAL_UINT32(4u, func->return_type_count);
    TEST_ASSERT_NOT_NULL(func->return_types);
    TEST_ASSERT_EQUAL_INT(OKM_TY_I8, func->return_types[0]);
    TEST_ASSERT_EQUAL_INT(OKM_TY_I16, func->return_types[1]);
    TEST_ASSERT_EQUAL_INT(OKM_TY_I32, func->return_types[2]);
    TEST_ASSERT_EQUAL_INT(OKM_TY_I64, func->return_types[3]);
}

void test_NewFunction_InitialState(void) {
    const OkmType ret_types[] = {OKM_TY_I64};
    OkmFunction* func = okm_new_function(&ctx, "f", ret_types, 1u);
    TEST_ASSERT_NULL(func->params);
    TEST_ASSERT_EQUAL_UINT32(0u, func->param_count);
    TEST_ASSERT_NULL(func->block_head);
    TEST_ASSERT_NULL(func->block_tail);
    TEST_ASSERT_EQUAL_UINT32(0u, func->next_val_id);
    TEST_ASSERT_EQUAL_UINT32(0u, func->next_block_id);
}

void test_NewFunction_MultipleIndependent(void) {
    const OkmType ret1[] = {OKM_TY_I32};
    const OkmType ret2[] = {OKM_TY_I64};
    OkmFunction* f1 = okm_new_function(&ctx, "f1", ret1, 1u);
    OkmFunction* f2 = okm_new_function(&ctx, "f2", ret2, 1u);
    TEST_ASSERT_NOT_EQUAL(f1, f2);
    TEST_ASSERT_EQUAL_STRING("f1", f1->name);
    TEST_ASSERT_EQUAL_STRING("f2", f2->name);
    TEST_ASSERT_EQUAL_UINT32(1u, f1->return_type_count);
    TEST_ASSERT_EQUAL_INT(OKM_TY_I32, f1->return_types[0]);
    TEST_ASSERT_EQUAL_UINT32(1u, f2->return_type_count);
    TEST_ASSERT_EQUAL_INT(OKM_TY_I64, f2->return_types[0]);
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_NewFunction_Name);
    RUN_TEST(test_NewFunction_ReturnTypeSingle);
    RUN_TEST(test_NewFunction_ReturnTypeNone);
    RUN_TEST(test_NewFunction_ReturnTypeMultiple);
    RUN_TEST(test_NewFunction_InitialState);
    RUN_TEST(test_NewFunction_MultipleIndependent);
    return UNITY_END();
}
