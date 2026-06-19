#include "x86_64/emit.h"

#include <stdio.h>
#include <string.h>

#include "context.h"
#include "ir.h"
#include "ockham/ockham.h"
#include "unity.h"

void setUp(void) {}
void tearDown(void) {}

void test_LowerFunction_MainReturnOne(void) {
    OkmContext* ctx = okm_new_context(OKM_ARCH_X86_64, OKM_OS_LINUX);
    TEST_ASSERT_NOT_NULL(ctx);

    const OkmType ret_types[] = {OKM_TY_I32};
    OkmFunction* func = okm_new_function(ctx, "main", ret_types, 1u);
    TEST_ASSERT_NOT_NULL(func);

    OkmBlock* block = okm_new_block(ctx, func);
    TEST_ASSERT_NOT_NULL(block);

    OkmValue* ret_val = okm_emit_const_int(ctx, block, 1);
    TEST_ASSERT_NOT_NULL(ret_val);
    ret_val->type = OKM_TY_I32;

    OkmInstr* ret_instr = okm_emit_ret(ctx, block, &ret_val, 1);
    TEST_ASSERT_NOT_NULL(ret_instr);

    FILE* fp = tmpfile();
    TEST_ASSERT_NOT_NULL(fp);

    okm_lower_function_x86_64(ctx, func, fp);

    rewind(fp);

    char buffer[1024];
    size_t bytes_read = fread(buffer, 1, sizeof(buffer) - 1, fp);
    buffer[bytes_read] = '\0';

    fclose(fp);

    const char* expected =
        "    .global main\n"
        "main:\n"
        "    pushq %rbp\n"
        "    movq %rsp, %rbp\n"
        "    subq $16, %rsp\n"
        "    movl $1, -8(%rbp)\n"
        "    movl -8(%rbp), %eax\n"
        "    movq %rbp, %rsp\n"
        "    popq %rbp\n"
        "    ret\n";

    TEST_ASSERT_EQUAL_STRING(expected, buffer);

    okm_destroy_context(ctx);
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_LowerFunction_MainReturnOne);
    return UNITY_END();
}
