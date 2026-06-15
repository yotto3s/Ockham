#include <stdint.h>

#include "arena.h"
#include "context.h"
#include "ir.h"
#include "ockham/ockham.h"
#include "unity.h"

static OkmContext ctx;
static OkmFunction* func;
static OkmBlock* block;
static OkmValue dst;
static OkmValue lhs;
static OkmValue rhs;

void setUp(void) {
    okm_arena_init(&ctx.arena);
    func = okm_emit_function(&ctx, "test_func", OKM_TY_I32);
    block = okm_emit_block(&ctx, func);
    dst.id = 0;
    dst.def = NULL;
    lhs.id = 1;
    lhs.def = NULL;
    rhs.id = 2;
    rhs.def = NULL;
}

void tearDown(void) { okm_arena_destroy(&ctx.arena); }

/* --- okm_emit_const_int --- */

void test_EmitConstInt_Op(void) {
    OkmInstr* instr = okm_emit_const_int(&ctx, block, &dst, 42u);
    TEST_ASSERT_NOT_NULL(instr);
    TEST_ASSERT_EQUAL_INT(OKM_OP_CONST, instr->op);
}

void test_EmitConstInt_Value(void) {
    OkmInstr* instr = okm_emit_const_int(&ctx, block, &dst, 1234u);
    TEST_ASSERT_EQUAL_UINT64(1234u, instr->as.imm.val.i);
}

void test_EmitConstInt_SetsDefOnDst(void) {
    OkmInstr* instr = okm_emit_const_int(&ctx, block, &dst, 0u);
    TEST_ASSERT_EQUAL_PTR(instr, dst.def);
}

void test_EmitConstInt_NullDst(void) {
    OkmInstr* instr = okm_emit_const_int(&ctx, block, NULL, 99u);
    TEST_ASSERT_NOT_NULL(instr);
    TEST_ASSERT_EQUAL_INT(OKM_OP_CONST, instr->op);
}

/* --- okm_emit_alu --- */

void test_EmitAlu_Op(void) {
    OkmInstr* instr = okm_emit_alu(&ctx, block, OKM_OP_ADD, &dst, &lhs, &rhs);
    TEST_ASSERT_NOT_NULL(instr);
    TEST_ASSERT_EQUAL_INT(OKM_OP_ADD, instr->op);
}

void test_EmitAlu_Operands(void) {
    OkmInstr* instr = okm_emit_alu(&ctx, block, OKM_OP_MUL, &dst, &lhs, &rhs);
    TEST_ASSERT_EQUAL_PTR(&dst, instr->as.alu.dst);
    TEST_ASSERT_EQUAL_PTR(&lhs, instr->as.alu.lhs);
    TEST_ASSERT_EQUAL_PTR(&rhs, instr->as.alu.rhs);
}

void test_EmitAlu_InvalidOpReturnsNull(void) {
    OkmInstr* instr = okm_emit_alu(&ctx, block, OKM_OP_RET, &dst, &lhs, &rhs);
    TEST_ASSERT_NULL(instr);
}

/* --- okm_emit_ret --- */

void test_EmitRet_Op(void) {
    OkmInstr* instr = okm_emit_ret(&ctx, block, &dst);
    TEST_ASSERT_NOT_NULL(instr);
    TEST_ASSERT_EQUAL_INT(OKM_OP_RET, instr->op);
}

void test_EmitRet_Val(void) {
    OkmInstr* instr = okm_emit_ret(&ctx, block, &dst);
    TEST_ASSERT_EQUAL_PTR(&dst, instr->as.ret.val);
}

void test_EmitRet_NullVal(void) {
    OkmInstr* instr = okm_emit_ret(&ctx, block, NULL);
    TEST_ASSERT_NOT_NULL(instr);
    TEST_ASSERT_NULL(instr->as.ret.val);
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_EmitConstInt_Op);
    RUN_TEST(test_EmitConstInt_Value);
    RUN_TEST(test_EmitConstInt_SetsDefOnDst);
    RUN_TEST(test_EmitConstInt_NullDst);
    RUN_TEST(test_EmitAlu_Op);
    RUN_TEST(test_EmitAlu_Operands);
    RUN_TEST(test_EmitAlu_InvalidOpReturnsNull);
    RUN_TEST(test_EmitRet_Op);
    RUN_TEST(test_EmitRet_Val);
    RUN_TEST(test_EmitRet_NullVal);
    return UNITY_END();
}
