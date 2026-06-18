#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "arena.h"
#include "context.h"
#include "ir.h"
#include "ockham/ockham.h"

static void unimplemented() {
    fprintf(stderr, "unimplemented!\n");
    exit(1);
}

static char* okm_op_to_string(OkmOp const op) {
    switch (op) {
        case OKM_OP_CONST:
            return "OKM_OP_CONST";
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
        case OKM_OP_ADD:   /* fall through */
        case OKM_OP_SUB:   /* fall through */
        case OKM_OP_MUL:   /* fall through */
        case OKM_OP_SDIV:  /* fall through */
        case OKM_OP_UDIV:  /* fall through */
        case OKM_OP_SREM:  /* fall through */
        case OKM_OP_UREM:  /* fall through */
        case OKM_OP_AND:   /* fall through */
        case OKM_OP_OR:    /* fall through */
        case OKM_OP_XOR:   /* fall through */
        case OKM_OP_SHL:   /* fall through */
        case OKM_OP_SHR:   /* fall through */
        case OKM_OP_EXTS:  /* fall through */
        case OKM_OP_EXTZ:  /* fall through */
        case OKM_OP_TRUNC: /* fall through */
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
    instr->as.imm.i = val;

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

OkmValue* okm_emit_const_float(OkmContext* const ctx, OkmBlock* const block,
                               const double val) {
    (void)ctx;
    (void)block;
    (void)val;
    unimplemented();
    return NULL;
}

OkmValue* okm_emit_alloca(OkmContext* const ctx, OkmBlock* const block,
                          const uint32_t bytes) {
    (void)ctx;
    (void)block;
    (void)bytes;
    unimplemented();
    return NULL;
}

OkmValue* okm_emit_load(OkmContext* const ctx, OkmBlock* const block,
                        const OkmType type, OkmValue* const ptr) {
    (void)ctx;
    (void)block;
    (void)type;
    (void)ptr;
    unimplemented();
    return NULL;
}

OkmInstr* okm_emit_store(OkmContext* const ctx, OkmBlock* const block,
                         OkmValue* const val, OkmValue* const ptr) {
    (void)ctx;
    (void)block;
    (void)val;
    (void)ptr;
    unimplemented();
    return NULL;
}

OkmInstr* okm_emit_jmp(OkmContext* const ctx, OkmBlock* const block,
                       OkmBlock* const target, OkmValue** const args,
                       const uint32_t arg_count) {
    (void)ctx;
    (void)block;
    (void)target;
    (void)args;
    (void)arg_count;
    unimplemented();
    return NULL;
}

OkmInstr* okm_emit_jnz(OkmContext* const ctx, OkmBlock* const block,
                       OkmValue* const cond, OkmBlock* const target_true,
                       OkmValue** const args_true,
                       const uint32_t arg_count_true,
                       OkmBlock* const target_false,
                       OkmValue** const args_false,
                       const uint32_t arg_count_false) {
    (void)ctx;
    (void)block;
    (void)cond;
    (void)target_true;
    (void)args_true;
    (void)arg_count_true;
    (void)target_false;
    (void)args_false;
    (void)arg_count_false;
    unimplemented();
    return NULL;
}

OkmValue* okm_emit_call(OkmContext* const ctx, OkmBlock* const block,
                        const OkmType return_type, OkmValue* const callee,
                        OkmValue** const args, const uint32_t arg_count) {
    (void)ctx;
    (void)block;
    (void)return_type;
    (void)callee;
    (void)args;
    (void)arg_count;
    unimplemented();
    return NULL;
}

OkmValue* okm_emit_syscall(OkmContext* const ctx, OkmBlock* const block,
                           OkmValue* const sys_num, OkmValue** const args,
                           const uint32_t arg_count) {
    if (arg_count > 6) {
        fprintf(stderr, "Error: syscall cannot have more than 6 arguments\n");
        return NULL;
    }

    OkmInstr* instr = okm_alloc_instr(ctx, block);
    OkmValue* dst = okm_alloc_value_reg(ctx, block, instr);

    instr->op = OKM_OP_SYSCALL;
    instr->as.syscall.dst = dst;
    instr->as.syscall.sys_num = sys_num;
    instr->as.syscall.arg_count = arg_count;

    for (uint32_t i = 0u; i < arg_count; ++i) {
        instr->as.syscall.args[i] = args[i];
    }

    return dst;
}
