#ifndef OCKHAM_LIB_HELPERS_H_
#define OCKHAM_LIB_HELPERS_H_
#include <stdbool.h>

#include "ockham/ockham.h"

static inline bool is_binary_op(const OkmOp op) {
    switch (op) {
        case OKM_OP_ADD:
        case OKM_OP_SUB:
        case OKM_OP_MUL:
        case OKM_OP_SDIV:
        case OKM_OP_UDIV:
        case OKM_OP_SREM:
        case OKM_OP_UREM:
        case OKM_OP_AND:
        case OKM_OP_OR:
        case OKM_OP_XOR:
        case OKM_OP_SHL:
        case OKM_OP_SHR:
            return true;
        default:
            return false;
    }
}

static inline bool is_unary_op(const OkmOp op) {
    switch (op) {
        case OKM_OP_EXTS:
        case OKM_OP_EXTZ:
        case OKM_OP_TRUNC:
            return true;
        default:
            return false;
    }
}

#endif  // OCKHAM_LIB_HELPERS_H_
