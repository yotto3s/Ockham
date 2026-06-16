#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

#include "arena.h"
#include "context.h"
#include "ir.h"
#include "ockham/ockham.h"

static char* okm_op_to_string(OkmOp const op) {
    switch (op) {
        case OKM_OP_CONST:
            return "OKM_OP_CONST";
        case OKM_OP_FCONST:
            return "OKM_OP_FCONST";
        case OKM_OP_ADD:
            return "OKM_OP_ADD";
        case OKM_OP_SUB:
            return "OKM_OP_SUB";
        case OKM_OP_MUL:
            return "OKM_OP_MUL";
        case OKM_OP_SDIV:
            return "OKM_OP_SDIV";
        case OKM_OP_UDIV:
            return "OKM_OP_UDIV";
        case OKM_OP_SREM:
            return "OKM_OP_SREM";
        case OKM_OP_UREM:
            return "OKM_OP_UREM";
        case OKM_OP_FADD:
            return "OKM_OP_FADD";
        case OKM_OP_FSUB:
            return "OKM_OP_FSUB";
        case OKM_OP_FMUL:
            return "OKM_OP_FMUL";
        case OKM_OP_FDIV:
            return "OKM_OP_FDIV";
        case OKM_OP_AND:
            return "OKM_OP_AND";
        case OKM_OP_OR:
            return "OKM_OP_OR";
        case OKM_OP_XOR:
            return "OKM_OP_XOR";
        case OKM_OP_SHL:
            return "OKM_OP_SHL";
        case OKM_OP_SHR:
            return "OKM_OP_SHR";
        case OKM_OP_EXTS:
            return "OKM_OP_EXTS";
        case OKM_OP_EXTZ:
            return "OKM_OP_EXTZ";
        case OKM_OP_TRUNC:
            return "OKM_OP_TRUNC";
        case OKM_OP_F2I:
            return "OKM_OP_F2I";
        case OKM_OP_I2F:
            return "OKM_OP_I2F";
        case OKM_OP_ALLOCA:
            return "OKM_OP_ALLOCA";
        case OKM_OP_LOAD:
            return "OKM_OP_LOAD";
        case OKM_OP_STORE:
            return "OKM_OP_STORE";
        case OKM_OP_JMP:
            return "OKM_OP_JMP";
        case OKM_OP_JNZ:
            return "OKM_OP_JNZ";
        case OKM_OP_CALL:
            return "OKM_OP_CALL";
        case OKM_OP_RET:
            return "OKM_OP_RET";
        default:
            return "unknown";
    }

    return "unknown";
}

static bool okm_is_alu_op(OkmOp const op) {
    switch (op) {
        case OKM_OP_ADD:    // fall through
        case OKM_OP_SUB:    // fall through
        case OKM_OP_MUL:    // fall through
        case OKM_OP_SDIV:   // fall through
        case OKM_OP_UDIV:   // fall through
        case OKM_OP_SREM:   // fall through
        case OKM_OP_UREM:   // fall through
        case OKM_OP_FADD:   // fall through
        case OKM_OP_FSUB:   // fall through
        case OKM_OP_FMUL:   // fall through
        case OKM_OP_FDIV:   // fall through
        case OKM_OP_AND:    // fall through
        case OKM_OP_OR:     // fall through
        case OKM_OP_XOR:    // fall through
        case OKM_OP_SHL:    // fall through
        case OKM_OP_SHR:    // fall through
        case OKM_OP_EXTS:   // fall through
        case OKM_OP_EXTZ:   // fall through
        case OKM_OP_TRUNC:  // fall through
        case OKM_OP_F2I:    // fall through
        case OKM_OP_I2F:    // fall through
            return true;
        default:
            return false;
    }
}

static OkmInstr* okm_alloc_instr(OkmContext* const ctx, OkmBlock* const block) {
    OkmInstr* const instr =
        (OkmInstr*)okm_arena_alloc(&ctx->arena, sizeof(OkmInstr));
    instr->next = NULL;
    instr->prev = block->instr_tail;
    if (block->instr_tail) {
        block->instr_tail->next = instr;
    } else {
        block->instr_head = instr;
    }
    block->instr_tail = instr;

    return instr;
}

static OkmValue* okm_alloc_value_reg(OkmContext* const ctx,
                                     OkmBlock* const block,
                                     OkmInstr* const instr) {
    OkmValue* val = (OkmValue*)okm_arena_alloc(&ctx->arena, sizeof(OkmValue));
    val->kind = OKM_VALUE_KIND_REG;
    val->as.reg.id = block->function->next_val_id++;
    val->as.reg.def = instr;

    return val;
}

OkmValue* okm_emit_const_int(OkmContext* const ctx, OkmBlock* const block,
                             const uint64_t val) {
    OkmInstr* const instr = okm_alloc_instr(ctx, block);
    OkmValue* const dst = okm_alloc_value_reg(ctx, block, instr);

    instr->op = OKM_OP_CONST;
    instr->as.imm.dst = dst;
    instr->as.imm.val.i = val;

    return dst;
}

OkmValue* okm_emit_alu(OkmContext* const ctx, OkmBlock* const block,
                       OkmOp const op, OkmValue* const lhs,
                       OkmValue* const rhs) {
    if (!okm_is_alu_op(op)) {
        fprintf(stderr, "Error: expected alu op, found %s\n",
                okm_op_to_string(op));
        return NULL;
    }
    OkmInstr* const instr = okm_alloc_instr(ctx, block);
    OkmValue* const dst = okm_alloc_value_reg(ctx, block, instr);

    instr->op = op;
    instr->as.alu.dst = dst;
    instr->as.alu.lhs = lhs;
    instr->as.alu.rhs = rhs;

    return dst;
}

OkmInstr* okm_emit_ret(OkmContext* const ctx, OkmBlock* const block,
                       OkmValue* const val) {
    OkmInstr* const instr = okm_alloc_instr(ctx, block);
    instr->op = OKM_OP_RET;
    instr->as.ret.val = val;

    return instr;
}
