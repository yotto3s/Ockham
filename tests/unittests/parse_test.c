#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ir.h"
#include "ockham/ockham.h"
#include "unity.h"

void setUp(void) {}
void tearDown(void) {}

static void assert_roundtrip(const char* const input_il) {
    OkmContext* ctx = okm_new_context(OKM_ARCH_X86_64, OKM_OS_LINUX);
    TEST_ASSERT_NOT_NULL(ctx);

    OkmFunction* func = okm_parse_function(ctx, input_il);
    TEST_ASSERT_NOT_NULL(func);

    char* buf = NULL;
    size_t size = 0;
    FILE* fp = open_memstream(&buf, &size);
    TEST_ASSERT_NOT_NULL(fp);

    okm_print_function(func, fp);
    fclose(fp);

    TEST_ASSERT_EQUAL_STRING(input_il, buf);

    free(buf);
    okm_destroy_context(ctx);
}

void test_ParseRoundtrip_Simple(void) {
    const char* il =
        "func @simple() {\n"
        "^block0:\n"
        "    ret \n"
        "}\n";
    assert_roundtrip(il);
}

void test_ParseRoundtrip_ParamsAndReturns(void) {
    const char* il =
        "func @add_nums(%0 : i32, %1 : i32) -> i32 {\n"
        "^block0:\n"
        "    %2 = add %0 %1 : i32\n"
        "    ret %2\n"
        "}\n";
    assert_roundtrip(il);
}

void test_ParseRoundtrip_AluOps(void) {
    const char* il =
        "func @alu_test(%0 : i32, %1 : i32) -> i32 {\n"
        "^block0:\n"
        "    %2 = add %0 %1 : i32\n"
        "    %3 = sub %2 %1 : i32\n"
        "    %4 = mul %3 %2 : i32\n"
        "    %5 = sdiv %4 %1 : i32\n"
        "    %6 = udiv %5 %0 : i32\n"
        "    %7 = srem %6 %1 : i32\n"
        "    %8 = urem %7 %0 : i32\n"
        "    %9 = and %8 %1 : i32\n"
        "    %10 = or %9 %0 : i32\n"
        "    %11 = xor %10 %1 : i32\n"
        "    %12 = shl %11 %0 : i32\n"
        "    %13 = shr %12 %1 : i32\n"
        "    ret %13\n"
        "}\n";
    assert_roundtrip(il);
}

void test_ParseRoundtrip_UnaryOps(void) {
    const char* il =
        "func @unary_test(%0 : i32) -> i64 {\n"
        "^block0:\n"
        "    %1 = exts %0 : i64\n"
        "    %2 = extz %0 : i64\n"
        "    %3 = trunc %2 : i32\n"
        "    ret %2\n"
        "}\n";
    assert_roundtrip(il);
}

void test_ParseRoundtrip_MemOps(void) {
    const char* il =
        "func @mem_test(%0 : i32) -> i32 {\n"
        "^block0:\n"
        "    %1 = alloc 16\n"
        "    store %0, %1 : i32\n"
        "    %2 = load %1 : i32\n"
        "    ret %2\n"
        "}\n";
    assert_roundtrip(il);
}

void test_ParseRoundtrip_ControlFlow(void) {
    const char* il =
        "func @control_flow(%0 : i32) -> i32 {\n"
        "^block0:\n"
        "    br %0, ^block1, ^block2\n"
        "^block1:\n"
        "    %1 = const_int 100 : i32\n"
        "    jmp ^block3\n"
        "^block2:\n"
        "    %2 = const_int 200 : i32\n"
        "    jmp ^block3\n"
        "^block3:\n"
        "    %3 = const_int 0 : i32\n"
        "    ret %3\n"
        "}\n";
    assert_roundtrip(il);
}

void test_ParseRoundtrip_Call(void) {
    const char* il =
        "func @call_test(%0 : i32) -> i32 {\n"
        "^block0:\n"
        "    call @void_func()\n"
        "    %1 = call @func_one_arg(%0) : i32\n"
        "    %2, %3 = call @func_two_returns(%1, %0) : i32, i64\n"
        "    ret %2\n"
        "}\n";
    assert_roundtrip(il);
}

void test_ParseRoundtrip_Syscall(void) {
    const char* il =
        "func @syscall_test(%0 : i64, %1 : i64) -> i64 {\n"
        "^block0:\n"
        "    syscall %0(%1)\n"
        "    %2 = syscall %0(%1, %1) : i64\n"
        "    ret %2\n"
        "}\n";
    assert_roundtrip(il);
}

void test_ParseRoundtrip_BlockArguments(void) {
    const char* il =
        "func @block_args(%0 : i32, %1 : i32) -> i32 {\n"
        "^block0:\n"
        "    br %0, ^block1(%0, %1), ^block2(%1)\n"
        "^block1(%2 : i32, %3 : i32):\n"
        "    %4 = add %2 %3 : i32\n"
        "    jmp ^block3(%4)\n"
        "^block2(%5 : i32):\n"
        "    jmp ^block3(%5)\n"
        "^block3(%6 : i32):\n"
        "    ret %6\n"
        "}\n";
    assert_roundtrip(il);
}

void test_ParseRejectsLargeRegisterId(void) {
    OkmContext* ctx = okm_new_context(OKM_ARCH_X86_64, OKM_OS_LINUX);
    TEST_ASSERT_NOT_NULL(ctx);

    /* 4294967232 = UINT32_MAX - 63; reg_id + 64 wraps to 0 pre-fix. */
    const char* il =
        "func @test(%4294967232 : i32) -> i32 {\n"
        "^block0:\n"
        "    ret %4294967232\n"
        "}\n";

    OkmFunction* func = okm_parse_function(ctx, il);
    TEST_ASSERT_NULL(func);

    okm_destroy_context(ctx);
}

void test_ParseRejectsAllocBytesOverflow(void) {
    OkmContext* ctx = okm_new_context(OKM_ARCH_X86_64, OKM_OS_LINUX);
    TEST_ASSERT_NOT_NULL(ctx);

    /* 5000000000 > UINT32_MAX; would truncate to 705032704 silently pre-fix. */
    const char* il =
        "func @test() {\n"
        "^block0:\n"
        "    %0 = alloc 5000000000\n"
        "    ret \n"
        "}\n";

    OkmFunction* func = okm_parse_function(ctx, il);
    TEST_ASSERT_NULL(func);

    okm_destroy_context(ctx);
}

void test_ParseRejectsDuplicateBlockLabel(void) {
    OkmContext* ctx = okm_new_context(OKM_ARCH_X86_64, OKM_OS_LINUX);
    TEST_ASSERT_NOT_NULL(ctx);

    const char* il =
        "func @test() {\n"
        "^block0:\n"
        "^block0:\n"
        "    ret \n"
        "}\n";

    OkmFunction* func = okm_parse_function(ctx, il);
    TEST_ASSERT_NULL(func);

    okm_destroy_context(ctx);
}

void test_ParseRoundtrip_PtrType(void) {
    const char* il =
        "func @ptr_test(%0 : ptr) -> ptr {\n"
        "^block0:\n"
        "    %1 = alloc 8\n"
        "    store %0, %1 : ptr\n"
        "    %2 = load %1 : ptr\n"
        "    ret %2\n"
        "}\n";
    assert_roundtrip(il);
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_ParseRoundtrip_Simple);
    RUN_TEST(test_ParseRoundtrip_ParamsAndReturns);
    RUN_TEST(test_ParseRoundtrip_AluOps);
    RUN_TEST(test_ParseRoundtrip_UnaryOps);
    RUN_TEST(test_ParseRoundtrip_MemOps);
    RUN_TEST(test_ParseRoundtrip_ControlFlow);
    RUN_TEST(test_ParseRoundtrip_Call);
    RUN_TEST(test_ParseRoundtrip_Syscall);
    RUN_TEST(test_ParseRoundtrip_BlockArguments);
    RUN_TEST(test_ParseRejectsLargeRegisterId);
    RUN_TEST(test_ParseRejectsAllocBytesOverflow);
    RUN_TEST(test_ParseRejectsDuplicateBlockLabel);
    RUN_TEST(test_ParseRoundtrip_PtrType);
    return UNITY_END();
}
