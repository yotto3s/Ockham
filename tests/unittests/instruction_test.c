#include <stdint.h>

#include "arena.h"
#include "context.h"
#include "ir.h"
#include "ockham/ockham.h"
#include "unity.h"

static OkmContext ctx;
static OkmFunction* func;
static OkmBlock* block;

void setUp(void) {
    okm_arena_init(&ctx.arena);
    func = okm_new_function(&ctx, "test_func", OKM_TY_I32);
    block = okm_new_block(&ctx, func);
}

void tearDown(void) { okm_arena_destroy(&ctx.arena); }

/* --- okm_emit_const_int --- */

void test_EmitConstInt_ReturnsNonNull(void) {
    OkmValue* val = okm_emit_const_int(&ctx, block, 42u);
    TEST_ASSERT_NOT_NULL(val);
}

void test_EmitConstInt_KindIsReg(void) {
    OkmValue* val = okm_emit_const_int(&ctx, block, 42u);
    TEST_ASSERT_EQUAL_INT(OKM_VALUE_KIND_REG, val->kind);
}

void test_EmitConstInt_RegIdAssigned(void) {
    OkmValue* val = okm_emit_const_int(&ctx, block, 42u);
    TEST_ASSERT_EQUAL_UINT32(0u, val->as.reg.id);
}

void test_EmitConstInt_RegIdIncrementsAcrossCalls(void) {
    OkmValue* v1 = okm_emit_const_int(&ctx, block, 1u);
    OkmValue* v2 = okm_emit_const_int(&ctx, block, 2u);
    TEST_ASSERT_EQUAL_UINT32(0u, v1->as.reg.id);
    TEST_ASSERT_EQUAL_UINT32(1u, v2->as.reg.id);
}

void test_EmitConstInt_DefOpIsConst(void) {
    OkmValue* val = okm_emit_const_int(&ctx, block, 42u);
    TEST_ASSERT_NOT_NULL(val->as.reg.def);
    TEST_ASSERT_EQUAL_INT(OKM_OP_CONST, val->as.reg.def->op);
}

void test_EmitConstInt_DefStoresValue(void) {
    OkmValue* val = okm_emit_const_int(&ctx, block, 1234u);
    TEST_ASSERT_EQUAL_UINT64(1234u, val->as.reg.def->as.imm.val.i);
}

void test_EmitConstInt_DefDstPointsBack(void) {
    OkmValue* val = okm_emit_const_int(&ctx, block, 0u);
    TEST_ASSERT_EQUAL_PTR(val, val->as.reg.def->as.imm.dst);
}

/* --- okm_emit_alu --- */

void test_EmitAlu_ReturnsNonNull(void) {
    OkmValue* lhs = okm_emit_const_int(&ctx, block, 1u);
    OkmValue* rhs = okm_emit_const_int(&ctx, block, 2u);
    OkmValue* val = okm_emit_alu(&ctx, block, OKM_OP_ADD, lhs, rhs);
    TEST_ASSERT_NOT_NULL(val);
}

void test_EmitAlu_KindIsReg(void) {
    OkmValue* lhs = okm_emit_const_int(&ctx, block, 1u);
    OkmValue* rhs = okm_emit_const_int(&ctx, block, 2u);
    OkmValue* val = okm_emit_alu(&ctx, block, OKM_OP_ADD, lhs, rhs);
    TEST_ASSERT_EQUAL_INT(OKM_VALUE_KIND_REG, val->kind);
}

void test_EmitAlu_DefOp(void) {
    OkmValue* lhs = okm_emit_const_int(&ctx, block, 1u);
    OkmValue* rhs = okm_emit_const_int(&ctx, block, 2u);
    OkmValue* val = okm_emit_alu(&ctx, block, OKM_OP_MUL, lhs, rhs);
    TEST_ASSERT_NOT_NULL(val->as.reg.def);
    TEST_ASSERT_EQUAL_INT(OKM_OP_MUL, val->as.reg.def->op);
}

void test_EmitAlu_DefOperands(void) {
    OkmValue* lhs = okm_emit_const_int(&ctx, block, 10u);
    OkmValue* rhs = okm_emit_const_int(&ctx, block, 20u);
    OkmValue* val = okm_emit_alu(&ctx, block, OKM_OP_ADD, lhs, rhs);
    TEST_ASSERT_EQUAL_PTR(lhs, val->as.reg.def->as.alu.lhs);
    TEST_ASSERT_EQUAL_PTR(rhs, val->as.reg.def->as.alu.rhs);
}

void test_EmitAlu_InvalidOpReturnsNull(void) {
    OkmValue* lhs = okm_emit_const_int(&ctx, block, 1u);
    OkmValue* rhs = okm_emit_const_int(&ctx, block, 2u);
    OkmValue* val = okm_emit_alu(&ctx, block, OKM_OP_RET, lhs, rhs);
    TEST_ASSERT_NULL(val);
}

/* --- okm_emit_ret --- */

void test_EmitRet_Op(void) {
    OkmValue* retval = okm_emit_const_int(&ctx, block, 0u);
    OkmInstr* instr = okm_emit_ret(&ctx, block, retval);
    TEST_ASSERT_NOT_NULL(instr);
    TEST_ASSERT_EQUAL_INT(OKM_OP_RET, instr->op);
}

void test_EmitRet_Val(void) {
    OkmValue* retval = okm_emit_const_int(&ctx, block, 0u);
    OkmInstr* instr = okm_emit_ret(&ctx, block, retval);
    TEST_ASSERT_EQUAL_PTR(retval, instr->as.ret.val);
}

void test_EmitRet_NullVal(void) {
    OkmInstr* instr = okm_emit_ret(&ctx, block, NULL);
    TEST_ASSERT_NOT_NULL(instr);
    TEST_ASSERT_NULL(instr->as.ret.val);
}

int main(void) {
    UNITY_BEGIN();
    RUN_TEST(test_EmitConstInt_ReturnsNonNull);
    RUN_TEST(test_EmitConstInt_KindIsReg);
    RUN_TEST(test_EmitConstInt_RegIdAssigned);
    RUN_TEST(test_EmitConstInt_RegIdIncrementsAcrossCalls);
    RUN_TEST(test_EmitConstInt_DefOpIsConst);
    RUN_TEST(test_EmitConstInt_DefStoresValue);
    RUN_TEST(test_EmitConstInt_DefDstPointsBack);
    RUN_TEST(test_EmitAlu_ReturnsNonNull);
    RUN_TEST(test_EmitAlu_KindIsReg);
    RUN_TEST(test_EmitAlu_DefOp);
    RUN_TEST(test_EmitAlu_DefOperands);
    RUN_TEST(test_EmitAlu_InvalidOpReturnsNull);
    RUN_TEST(test_EmitRet_Op);
    RUN_TEST(test_EmitRet_Val);
    RUN_TEST(test_EmitRet_NullVal);
    return UNITY_END();
}
