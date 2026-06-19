#ifndef OCKHAM_INCLUDE_OCKHAM_OCKHAM_H_
#define OCKHAM_INCLUDE_OCKHAM_OCKHAM_H_

/* The only types the IL knows about. Everything is just bits. */
typedef enum {
    OKM_TY_I8,
    OKM_TY_I16,
    OKM_TY_I32,
    OKM_TY_I64,
} OkmType;

/* Opcodes contain the signedness and operation logic. */
typedef enum {
    /* Constants */
    OKM_OP_CONST,

    /* Integer Math */
    OKM_OP_ADD,
    OKM_OP_SUB,
    OKM_OP_MUL,
    OKM_OP_SDIV,
    OKM_OP_UDIV,
    OKM_OP_SREM,
    OKM_OP_UREM,

    /* Bitwise (Always unsigned natively) */
    OKM_OP_AND,
    OKM_OP_OR,
    OKM_OP_XOR,
    OKM_OP_SHL,
    OKM_OP_SHR,

    /* Conversions */
    OKM_OP_EXTS,  /* Sign extend */
    OKM_OP_EXTZ,  /* Zero extend */
    OKM_OP_TRUNC, /* Truncate */

    /* Memory Operations */
    OKM_OP_ALLOCA, /* Allocates stack space */
    OKM_OP_LOAD,   /* Reads from pointer */
    OKM_OP_STORE,  /* Writes to pointer */

    /* Control Flow */
    OKM_OP_JMP,    /* Unconditional jump */
    OKM_OP_BRANCH, /* Jump if not zero */
    OKM_OP_CALL,
    OKM_OP_RET,
    OKM_OP_SYSCALL
} OkmOp;

typedef enum {
    OKM_VALUE_KIND_REG,             /* SSA register */
    OKM_VALUE_KIND_GLOBAL_SYMBOL,   /* Global variable symbol */
    OKM_VALUE_KIND_FUNCTION_SYMBOL, /* Function symbol */
} OkmValueKind;

typedef enum { OKM_ARCH_X86_64, OKM_ARCH_AARCH64 } OkmArch;

typedef enum {
    OKM_OS_LINUX,
    OKM_OS_MACOS,
    OKM_OS_WINDOWS,
    OKM_OS_FREESTANDING
} OkmOS;

typedef struct OkmValue OkmValue;
typedef struct OkmBlock OkmBlock;
typedef struct OkmInstr OkmInstr;
typedef struct OkmFunction OkmFunction;
typedef struct OkmContext OkmContext;

OkmContext* okm_new_context(const OkmArch arch, const OkmOS os);
void okm_destroy_context(OkmContext* const ctx);
OkmFunction* okm_new_function(OkmContext* const ctx, const char* const name,
                              const OkmType* return_types,
                              const uint32_t return_type_count);
OkmBlock* okm_new_block(OkmContext* const ctx, OkmFunction* const func);

/* Instructions */
OkmValue* okm_emit_const_int(OkmContext* const ctx, OkmBlock* const block,
                             const uint64_t val);
OkmValue* okm_emit_alu(OkmContext* const ctx, OkmBlock* const block,
                       OkmOp const op, OkmValue* const lhs,
                       OkmValue* const rhs);
OkmInstr* okm_emit_ret(OkmContext* const ctx, OkmBlock* const block,
                       OkmValue** const values, uint32_t value_count);
OkmValue* okm_emit_alloca(OkmContext* const ctx, OkmBlock* const block,
                          const uint32_t bytes);
OkmValue* okm_emit_load(OkmContext* const ctx, OkmBlock* const block,
                        const OkmType type, OkmValue* const ptr);
OkmInstr* okm_emit_store(OkmContext* const ctx, OkmBlock* const block,
                         OkmValue* const val, OkmValue* const ptr);
OkmInstr* okm_emit_jmp(OkmContext* const ctx, OkmBlock* const block,
                       OkmBlock* const target, OkmValue** const args,
                       const uint32_t arg_count);
OkmInstr* okm_emit_br(OkmContext* const ctx, OkmBlock* const block,
                      OkmValue* const cond, OkmBlock* const target_true,
                      OkmValue** const args_true, const uint32_t arg_count_true,
                      OkmBlock* const target_false, OkmValue** const args_false,
                      const uint32_t arg_count_false);
OkmValue* okm_emit_call(OkmContext* const ctx, OkmBlock* const block,
                        const OkmType return_type, OkmValue* const callee,
                        OkmValue** const args, const uint32_t arg_count);
OkmValue* okm_emit_syscall(OkmContext* const ctx, OkmBlock* const block,
                           OkmValue* const sys_num, OkmValue** const args,
                           const uint32_t arg_count);

#endif /* OCKHAM_INCLUDE_OCKHAM_OCKHAM_H_ */
