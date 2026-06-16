#include "context.h"

#include "ockham/ockham.h"
#include "unity.h"

void setUp(void) {}
void tearDown(void) {}

void test_NewContext_X86_64_Linux(void) {
    OkmContext* ctx = okm_new_context(OKM_ARCH_X86_64, OKM_OS_LINUX);
    TEST_ASSERT_NOT_NULL(ctx);
    TEST_ASSERT_EQUAL_INT(OKM_ARCH_X86_64, ctx->target.arch);
    TEST_ASSERT_EQUAL_INT(OKM_OS_LINUX, ctx->target.os);
    TEST_ASSERT_EQUAL_UINT8(8u, ctx->target.ptr_size);
    TEST_ASSERT_EQUAL_INT(OKM_TY_I64, ctx->target.isize_type);

    /* Make sure arena works */
    void* ptr = okm_arena_alloc(&ctx->arena, 64);
    TEST_ASSERT_NOT_NULL(ptr);

    okm_destroy_context(ctx);
}

void test_NewContext_Aarch64_Freestanding(void) {
    OkmContext* ctx = okm_new_context(OKM_ARCH_AARCH64, OKM_OS_FREESTANDING);
    TEST_ASSERT_NOT_NULL(ctx);
    TEST_ASSERT_EQUAL_INT(OKM_ARCH_AARCH64, ctx->target.arch);
    TEST_ASSERT_EQUAL_INT(OKM_OS_FREESTANDING, ctx->target.os);
    TEST_ASSERT_EQUAL_UINT8(8u, ctx->target.ptr_size);
    TEST_ASSERT_EQUAL_INT(OKM_TY_I64, ctx->target.isize_type);

    okm_destroy_context(ctx);
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_NewContext_X86_64_Linux);
    RUN_TEST(test_NewContext_Aarch64_Freestanding);
    return UNITY_END();
}
