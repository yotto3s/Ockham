#ifndef OCKHAM_INCLUDE_OCKHAM_OCKHAM_H_
#define OCKHAM_INCLUDE_OCKHAM_OCKHAM_H_

/* The only types the IL knows about. Everything is just bits. */
typedef enum {
    OKM_TY_I8,
    OKM_TY_I16,
    OKM_TY_I32,
    OKM_TY_I64,
    OKM_TY_F32,
    OKM_TY_F64
} OkmType;

/* Opcodes contain the signedness and operation logic. */
typedef enum {
    /* Constants */
    OKM_OP_CONST,
    OKM_OP_FCONST,

    /* Integer Math */
    OKM_OP_ADD,
    OKM_OP_SUB,
    OKM_OP_MUL,
    OKM_OP_SDIV,
    OKM_OP_UDIV,
    OKM_OP_SREM,
    OKM_OP_UREM,

    /* Floating Point Math */
    OKM_OP_FADD,
    OKM_OP_FSUB,
    OKM_OP_FMUL,
    OKM_OP_FDIV,

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
    OKM_OP_F2I,   /* Float to Int */
    OKM_OP_I2F,   /* Int to Float */

    /* Memory Operations */
    OKM_OP_ALLOCA, /* Allocates stack space */
    OKM_OP_LOAD,   /* Reads from pointer */
    OKM_OP_STORE,  /* Writes to pointer */

    /* Control Flow */
    OKM_OP_JMP, /* Unconditional jump */
    OKM_OP_JNZ, /* Jump if not zero */
    OKM_OP_CALL,
    OKM_OP_RET,
    OKM_OP_SYSCALL
} OkmOp;

typedef enum {
    OKM_VALUE_KIND_REG,             /* SSA register */
    OKM_VALUE_KIND_GLOBAL_SYMBOL,   /* Global variable symbol */
    OKM_VALUE_KIND_FUNCTION_SYMBOL, /* Function symbol */
} OkmValueKind;

typedef struct OkmValue OkmValue;
typedef struct OkmBlock OkmBlock;
typedef struct OkmInstr OkmInstr;
typedef struct OkmFunction OkmFunction;
typedef struct OkmContext OkmContext;

OkmValue* okm_emit_const_int(OkmContext* const ctx, OkmBlock* const block,
                             const uint64_t val);
OkmValue* okm_emit_alu(OkmContext* const ctx, OkmBlock* const block,
                       OkmOp const op, OkmValue* const lhs,
                       OkmValue* const rhs);
OkmInstr* okm_emit_ret(OkmContext* const ctx, OkmBlock* const block,
                       OkmValue* const val);
OkmFunction* okm_new_function(OkmContext* const ctx, const char* const name,
                              const OkmType return_type);
OkmBlock* okm_new_block(OkmContext* const ctx, OkmFunction* const func);

#endif /* OCKHAM_INCLUDE_OCKHAM_OCKHAM_H_ */
