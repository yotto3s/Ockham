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

    const char* il =
        "func @main() -> i32 {\n"
        "^block0:\n"
        "    %0 = const_int 1 : i32\n"
        "    ret %0\n"
        "}\n";

    OkmFunction* func = okm_parse_function(ctx, il);
    TEST_ASSERT_NOT_NULL(func);

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
