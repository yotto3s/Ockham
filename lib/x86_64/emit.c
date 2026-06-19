#include "x86_64/emit.h"

#include <stdint.h>
#include <stdio.h>

#include "ir.h"

/* Registers for arguments (caller-saved) */
static char* ARGUMENTS_REG64[6] = {"%rdi", "%rsi", "%rdx",
                                   "%rcx", "%r8",  "%r9"};
/* Registers for return values (caller-saved) */
static char* RETURN_VALUE_REG64[4] = {"%rax", "%rdx", "%rcx", "r8"};
/* Registers for temporal values (caller-saved) */
static char* TMP_REG64[2] = {"%r10", "%r11"};
/* Registers for general purpose (callee-saved) */
static char* GENERAL_REG64[5] = {"%rbx", "%r12", "%r13", "%r14", "%r15"};
/* Registers for base/stack pointer (callee-saved) */
static char* BASE_POINTER64 = "%rbp";
static char* STACK_POINTER64 = "%rsp";

/* 32 bit registers */
/* Registers for arguments (caller-saved) */
static char* ARGUMENTS_REG32[6] = {"%edi", "%esi", "%edx",
                                   "%ecx", "%r8d", "%r9d"};
/* Registers for return values (caller-saved) */
static char* RETURN_VALUE_REG32[4] = {"%eax", "%edx", "%ecx", "r8d"};
/* Registers for temporal values (caller-saved) */
static char* TMP_REG32[2] = {"%r10d", "%r11d"};
/* Registers for general purpose (callee-saved) */
static char* GENERAL_REG32[5] = {"%ebx", "%r12d", "%r13d", "%r14d", "%r15d"};
/* Registers for base/stack pointer (callee-saved) */
static char* BASE_POINTER32 = "%ebp";
static char* STACK_POINTER32 = "%esp";

/* 16 bit registers */
/* Registers for arguments (caller-saved) */
static char* ARGUMENTS_REG16[6] = {"%di", "%si", "%dx", "%cx", "%r8w", "%r9w"};
/* Registers for return values (caller-saved) */
static char* RETURN_VALUE_REG16[4] = {"%ax", "%dx", "%cx", "r8w"};
/* Registers for temporal values (caller-saved) */
static char* TMP_REG16[2] = {"%r10w", "%r11w"};
/* Registers for general purpose (callee-saved) */
static char* GENERAL_REG16[5] = {"%bx", "%r12w", "%r13w", "%r14w", "%r15w"};
/* Registers for base/stack pointer (callee-saved) */
static char* BASE_POINTER16 = "%bp";
static char* STACK_POINTER16 = "%sp";

/* 8 bit registers */
/* Registers for arguments (caller-saved) */
static char* ARGUMENTS_REG8[6] = {"%dil", "%sil", "%dl", "%cl", "%r8b", "%r9b"};
/* Registers for return values (caller-saved) */
static char* RETURN_VALUE_REG8[4] = {"%al", "%dl", "%cl", "r8b"};
/* Registers for temporal values (caller-saved) */
static char* TMP_REG8[2] = {"%r10b", "%r11b"};
/* Registers for general purpose (callee-saved) */
static char* GENERAL_REG8[5] = {"%bl", "%r12b", "%r13b", "%r14b", "%r15b"};
/* Registers for base/stack pointer (callee-saved) */
static char* BASE_POINTER8 = "%bpl";
static char* STACK_POINTER8 = "%spl";
/*
 * Calculate total stack space for given number of ssa registers
 * We align it to 16 bytes, which is required by the macOS/Linux ABI.
 */
static uint32_t get_required_stack_size(const uint32_t ssa_count) {
    uint32_t stack_size = ssa_count * 8u;
    if (stack_size % 16u != 0u) {
        stack_size += 8u;
    }

    return stack_size;
}

/*
 * Helper to calculate the stack offset for a virtual register ID.
 * ID 1 -> -8(%rbp)
 * ID 2 -> -16(%rbp)
 */
int32_t get_stack_offset(uint32_t reg_id) {
    /* Allocate 8 bytes per register for simplicity */
    return (int32_t)(reg_id + 1) * 8;
}

static void okm_lower_instruction(const OkmContext* const ctx,
                                  const OkmInstr* const instr, FILE* const fp) {
    switch (instr->op) {
        case OKM_OP_CONST: {
            int32_t out_off = get_stack_offset(instr->as.imm.dst->as.reg.id);
            uint64_t val = instr->as.imm.i;

            switch (instr->as.imm.dst->type) {
                case OKM_TY_I8:
                    fprintf(fp, "    movb $%llu, -%d(%s)\n", val, out_off,
                            BASE_POINTER64);
                    break;
                case OKM_TY_I16:
                    fprintf(fp, "    movw $%llu, -%d(%s)\n", val, out_off,
                            BASE_POINTER64);
                    break;
                case OKM_TY_I32:
                    fprintf(fp, "    movl $%llu, -%d(%s)\n", val, out_off,
                            BASE_POINTER64);
                    break;
                case OKM_TY_I64:
                case OKM_TY_PTR:
                    fprintf(fp, "    movq $%llu, -%d(%s)\n", val, out_off,
                            BASE_POINTER64);
                    break;
                default:
                    fprintf(stderr,
                            "Error: invalid value type for OKM_OP_CONST\n");
                    return;
            }
            break;
        }

        case OKM_OP_RET: {
            for (uint32_t i = 0u; i < instr->as.ret.value_count; ++i) {
                if (i >= 4) {
                    fprintf(stderr,
                            "Error: Max 4 return registers supported\n");
                    break;
                }

                int32_t ret_off =
                    get_stack_offset(instr->as.ret.values[i]->as.reg.id);

                switch (instr->as.ret.values[i]->type) {
                    case OKM_TY_I8:
                        fprintf(fp, "    movb -%d(%s), %s\n", ret_off,
                                BASE_POINTER64, RETURN_VALUE_REG8[i]);
                        break;
                    case OKM_TY_I16:
                        fprintf(fp, "    movw -%d(%s), %s\n", ret_off,
                                BASE_POINTER64, RETURN_VALUE_REG16[i]);
                        break;
                    case OKM_TY_I32:
                        fprintf(fp, "    movl -%d(%s), %s\n", ret_off,
                                BASE_POINTER64, RETURN_VALUE_REG32[i]);
                        break;
                    case OKM_TY_I64:
                    case OKM_TY_PTR:
                        fprintf(fp, "    movq -%d(%s), %s\n", ret_off,
                                BASE_POINTER64, RETURN_VALUE_REG64[i]);
                        break;
                }
            }
            fprintf(fp, "    movq %s, %s\n", BASE_POINTER64, STACK_POINTER64);
            fprintf(fp, "    popq %s\n", BASE_POINTER64);
            fprintf(fp, "    ret\n");
            break;
        }
        default: {
            fprintf(stderr, "Error: Unimplemented opcode in x86_64 backend\n");
            return;
        }
    }
}

static void okm_lower_block(const OkmContext* const ctx,
                            const OkmBlock* const block, FILE* const fp) {
    for (const OkmInstr* instr = block->instr_head; instr != NULL;
         instr = instr->next) {
        okm_lower_instruction(ctx, instr, fp);
    }
}

void okm_lower_function_x86_64(const OkmContext* const ctx,
                               const OkmFunction* const func, FILE* const fp) {
    /* emit function signatures */
    fprintf(fp, "    .global %s\n", func->name);
    fprintf(fp, "%s:\n", func->name);

    uint32_t stack_size = get_required_stack_size(func->next_val_id);

    fprintf(fp, "    pushq %s\n", BASE_POINTER64);
    fprintf(fp, "    movq %s, %s\n", STACK_POINTER64, BASE_POINTER64);
    if (stack_size > 0u) {
        fprintf(fp, "    subq $%d, %s\n", stack_size, STACK_POINTER64);
    }

    for (const OkmBlock* block = func->block_head; block != NULL;
         block = block->next) {
        okm_lower_block(ctx, block, fp);
    }
}
