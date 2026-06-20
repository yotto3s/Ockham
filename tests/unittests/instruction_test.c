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
    const OkmType ret_types[] = {OKM_TY_I32};
    func = okm_new_function(&ctx, "test_func", ret_types, 1u);
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
    TEST_ASSERT_EQUAL_UINT64(1234u, val->as.reg.def->as.imm.i);
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
    OkmInstr* instr = okm_emit_ret(&ctx, block, &retval, 1u);
    TEST_ASSERT_NOT_NULL(instr);
    TEST_ASSERT_EQUAL_INT(OKM_OP_RET, instr->op);
    TEST_ASSERT_EQUAL_INT(instr->as.ret.value_count, 1u);
}

void test_EmitRet_Val(void) {
    OkmValue* retval = okm_emit_const_int(&ctx, block, 0u);
    OkmInstr* instr = okm_emit_ret(&ctx, block, &retval, 1u);
    TEST_ASSERT_EQUAL_INT(instr->as.ret.value_count, 1u);
    TEST_ASSERT_EQUAL_PTR(retval, instr->as.ret.values[0]);
}

void test_EmitRet_NullVal(void) {
    OkmInstr* instr = okm_emit_ret(&ctx, block, NULL, 0u);
    TEST_ASSERT_NOT_NULL(instr);
    TEST_ASSERT_EQUAL_INT(instr->as.ret.value_count, 0u);
}

void test_EmitRet_MultipleVals(void) {
    OkmValue* vals[3];
    vals[0] = okm_emit_const_int(&ctx, block, 10u);
    vals[1] = okm_emit_const_int(&ctx, block, 20u);
    vals[2] = okm_emit_const_int(&ctx, block, 30u);
    OkmInstr* instr = okm_emit_ret(&ctx, block, vals, 3u);
    TEST_ASSERT_NOT_NULL(instr);
    TEST_ASSERT_EQUAL_INT(OKM_OP_RET, instr->op);
    TEST_ASSERT_EQUAL_INT(3u, instr->as.ret.value_count);
    TEST_ASSERT_EQUAL_PTR(vals[0], instr->as.ret.values[0]);
    TEST_ASSERT_EQUAL_PTR(vals[1], instr->as.ret.values[1]);
    TEST_ASSERT_EQUAL_PTR(vals[2], instr->as.ret.values[2]);
}

void test_EmitRet_TooManyVals(void) {
    OkmValue* vals[5];
    for (int i = 0; i < 5; ++i) {
        vals[i] = okm_emit_const_int(&ctx, block, (uint64_t)i);
    }
    OkmInstr* instr = okm_emit_ret(&ctx, block, vals, 5u);
    TEST_ASSERT_NULL(instr);
}

/* --- okm_emit_syscall --- */

void test_EmitSyscall_ReturnsNonNull(void) {
    OkmValue* args[2];
    args[0] = okm_emit_const_int(&ctx, block, 1u);
    args[1] = okm_emit_const_int(&ctx, block, 2u);
    OkmValue* sys_num = okm_emit_const_int(&ctx, block, 60u);
    OkmValue* val = okm_emit_syscall(&ctx, block, sys_num, args, 2u);
    TEST_ASSERT_NOT_NULL(val);
}

void test_EmitSyscall_OpcodeAndFields(void) {
    OkmValue* args[2];
    args[0] = okm_emit_const_int(&ctx, block, 10u);
    args[1] = okm_emit_const_int(&ctx, block, 20u);
    OkmValue* sys_num = okm_emit_const_int(&ctx, block, 60u);
    OkmValue* val = okm_emit_syscall(&ctx, block, sys_num, args, 2u);

    TEST_ASSERT_EQUAL_INT(OKM_VALUE_KIND_REG, val->kind);
    OkmInstr* instr = val->as.reg.def;
    TEST_ASSERT_NOT_NULL(instr);
    TEST_ASSERT_EQUAL_INT(OKM_OP_SYSCALL, instr->op);
    TEST_ASSERT_EQUAL_PTR(val, instr->as.syscall.dst);

    TEST_ASSERT_EQUAL_PTR(sys_num, instr->as.syscall.sys_num);

    TEST_ASSERT_EQUAL_UINT32(2u, instr->as.syscall.arg_count);
    TEST_ASSERT_EQUAL_PTR(args[0], instr->as.syscall.args[0]);
    TEST_ASSERT_EQUAL_PTR(args[1], instr->as.syscall.args[1]);
}

void test_EmitSyscall_TooManyArgsReturnsNull(void) {
    OkmValue* args[7];
    for (int i = 0; i < 7; ++i) {
        args[i] = okm_emit_const_int(&ctx, block, (uint64_t)i);
    }
    OkmValue* sys_num = okm_emit_const_int(&ctx, block, 60u);
    OkmValue* val = okm_emit_syscall(&ctx, block, sys_num, args, 7u);
    TEST_ASSERT_NULL(val);
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
    RUN_TEST(test_EmitRet_MultipleVals);
    RUN_TEST(test_EmitRet_TooManyVals);
    RUN_TEST(test_EmitSyscall_ReturnsNonNull);
    RUN_TEST(test_EmitSyscall_OpcodeAndFields);
    RUN_TEST(test_EmitSyscall_TooManyArgsReturnsNull);
    return UNITY_END();
}
