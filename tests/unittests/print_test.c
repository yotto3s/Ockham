#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "ir.h"
#include "ockham/ockham.h"
#include "unity.h"

void setUp(void) {}
void tearDown(void) {}

void test_PrintConstInt(void) {
    OkmContext* ctx = okm_new_context(OKM_ARCH_X86_64, OKM_OS_LINUX);
    const OkmType ret_types[] = {OKM_TY_I32};
    OkmFunction* func = okm_new_function(ctx, "test_func", ret_types, 1u);
    OkmBlock* block = okm_new_block(ctx, func);

    OkmValue* val = okm_emit_const_int(ctx, block, 42u);
    TEST_ASSERT_NOT_NULL(val);
    val->type = OKM_TY_I32;

    // Get the printed instruction
    char* buf = NULL;
    size_t size = 0;
    FILE* fp = open_memstream(&buf, &size);
    TEST_ASSERT_NOT_NULL(fp);

    okm_print_instr(val->as.reg.def, fp);
    fclose(fp);

    const char* expected = "    %0 = const_int 42 : i32\n";
    TEST_ASSERT_EQUAL_STRING(expected, buf);

    free(buf);
    okm_destroy_context(ctx);
}

void test_PrintAlu(void) {
    OkmContext* ctx = okm_new_context(OKM_ARCH_X86_64, OKM_OS_LINUX);
    const OkmType ret_types[] = {OKM_TY_I32};
    OkmFunction* func = okm_new_function(ctx, "test_func", ret_types, 1u);
    OkmBlock* block = okm_new_block(ctx, func);

    OkmValue* lhs = okm_emit_const_int(ctx, block, 10u);
    lhs->type = OKM_TY_I32;
    OkmValue* rhs = okm_emit_const_int(ctx, block, 20u);
    rhs->type = OKM_TY_I32;
    OkmValue* res = okm_emit_alu(ctx, block, OKM_OP_ADD, lhs, rhs);
    TEST_ASSERT_NOT_NULL(res);
    res->type = OKM_TY_I32;

    char* buf = NULL;
    size_t size = 0;
    FILE* fp = open_memstream(&buf, &size);
    TEST_ASSERT_NOT_NULL(fp);

    okm_print_instr(res->as.reg.def, fp);
    fclose(fp);

    const char* expected = "    %2 = add %0 %1 : i32\n";
    TEST_ASSERT_EQUAL_STRING(expected, buf);

    free(buf);
    okm_destroy_context(ctx);
}

void test_PrintFunction(void) {
    OkmContext* ctx = okm_new_context(OKM_ARCH_X86_64, OKM_OS_LINUX);
    const OkmType ret_types[] = {OKM_TY_I32};
    OkmFunction* func = okm_new_function(ctx, "add_two", ret_types, 1u);
    OkmBlock* block = okm_new_block(ctx, func);

    // Manually construct parameter value (normally set up during frontend
    // compilation) func @add_two(%0 : i32, %1 : i32) -> i32
    func->param_count = 2u;
    func->params = malloc(sizeof(OkmValue*) * 2);

    OkmValue* p0 = malloc(sizeof(OkmValue));
    p0->kind = OKM_VALUE_KIND_REG;
    p0->type = OKM_TY_I32;
    p0->as.reg.id = func->next_val_id++;
    p0->as.reg.def = NULL;

    OkmValue* p1 = malloc(sizeof(OkmValue));
    p1->kind = OKM_VALUE_KIND_REG;
    p1->type = OKM_TY_I32;
    p1->as.reg.id = func->next_val_id++;
    p1->as.reg.def = NULL;

    func->params[0] = p0;
    func->params[1] = p1;

    OkmValue* res = okm_emit_alu(ctx, block, OKM_OP_ADD, p0, p1);
    res->type = OKM_TY_I32;
    OkmValue* ret_vals[] = {res};
    okm_emit_ret(ctx, block, ret_vals, 1u);

    char* buf = NULL;
    size_t size = 0;
    FILE* fp = open_memstream(&buf, &size);
    TEST_ASSERT_NOT_NULL(fp);

    okm_print_function(func, fp);
    fclose(fp);

    const char* expected =
        "func @add_two(%0 : i32, %1 : i32) -> i32 {\n"
        "^block0:\n"
        "    %2 = add %0 %1 : i32\n"
        "    ret %2\n"
        "}\n";

    TEST_ASSERT_EQUAL_STRING(expected, buf);

    free(buf);
    // clean up manually allocated parameters
    free(p0);
    free(p1);
    free(func->params);
    okm_destroy_context(ctx);
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_PrintConstInt);
    RUN_TEST(test_PrintAlu);
    RUN_TEST(test_PrintFunction);
    return UNITY_END();
}
