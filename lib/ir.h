#ifndef OCKHAM_LIB_IR_H_
#define OCKHAM_LIB_IR_H_

#include <stdint.h>

#include "ockham/ockham.h"

/* An OkmValue represents a single, immutable SSA register. */
typedef struct OkmValue {
    OkmValueKind kind;
    /* The size/format of this register. For globals/funcs, this is a pointer
     * type like I64 */
    OkmType type;
    union {
        /* For REG kind */
        struct {
            uint32_t id;
            /* A pointer to the instruction that created this value. */
            OkmInstr* def;
        } reg;
        /* For GLOBAL and FUNCTION kind */
        struct {
            const char* symbol;
        } sym;
    } as;
} OkmValue;

typedef struct OkmInstr {
    OkmOp op;

    /* Intrusive linked list pointers for O(1) manipulation */
    struct OkmInstr* prev;
    struct OkmInstr* next;

    /* The Tagged Union holding instruction-specific data */
    union {
        /* Used for ADD, SUB, MUL, DIV, EXT, TRUNC, etc. */
        struct {
            OkmValue* dst;
            OkmValue* lhs;
            OkmValue* rhs; /* NULL for unary ops like EXT or TRUNC */
        } alu;

        /* Used for CONST and FCONST */
        struct {
            OkmValue* dst;
            uint64_t i;
        } imm;

        /* Used for ALLOC, LOAD, and STORE */
        struct {
            OkmValue* dst;  /* Dest for load/alloc, NULL for store */
            OkmValue* ptr;  /* The memory address to read/write */
            OkmValue* val;  /* The value to store (NULL for load/alloc) */
            uint32_t bytes; /* Size of allocation (for ALLOC only) */
        } mem;

        struct {
            OkmBlock* target;
            OkmValue** args;
            uint32_t arg_count;
        } jmp;

        /* Used for BRANCH */
        struct {
            OkmValue* cond;

            OkmBlock* target_true;
            OkmValue** args_true; /* Array of arguments passed to true block */
            uint32_t arg_count_true;

            OkmBlock* target_false;
            OkmValue**
                args_false; /* Array of arguments passed to false block */
            uint32_t arg_count_false;
        } br;

        /* Used for RET */
        struct {
            OkmValue* values[4]; /* NULL for void return */
            uint32_t value_count;
        } ret;

        /* Used for CALL */
        struct {
            OkmValue* func;
            OkmValue** dsts;
            uint32_t dst_count;
            OkmValue** args;
            uint32_t arg_count;
        } call;

        /* Used for syscall */
        struct {
            OkmValue* dst;      /* The result of the syscall */
            OkmValue* sys_num;  /* The syscall number */
            OkmValue* args[6];  /* Up to 6 arguments */
            uint32_t arg_count; /* How many arguments are actually used */
        } syscall;

    } as;
} OkmInstr;

typedef struct OkmBlock {
    uint32_t id; /* Block ID (e.g., @block_4) */

    /* MLIR-style Block Arguments (acting as phi destinations) */
    OkmValue** params; /* Array of SSA values this block requires */
    uint32_t param_count;

    /* Parent function */
    OkmFunction* function;

    /* Intrusive linked list of instructions */
    OkmInstr* instr_head;
    OkmInstr* instr_tail;

    /* Intrusive linked list of blocks inside a function */
    OkmBlock* prev;
    OkmBlock* next;
} OkmBlock;

typedef struct OkmFunction {
    const char* name; /* Interned string pointer */
    OkmType* return_types;
    uint32_t return_type_count;

    /* Standard function arguments */
    OkmValue** params;
    uint32_t param_count;

    /* Intrusive linked list of blocks */
    OkmBlock* block_head;
    OkmBlock* block_tail;

    /* Counters for generating unique IDs during compilation */
    uint32_t next_val_id;
    uint32_t next_block_id;
} OkmFunction;

#endif  // OCKHAM_LIB_IR_H_
