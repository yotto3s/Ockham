#ifndef OCKHAM_LIB_CONTEXT_H_
#define OCKHAM_LIB_CONTEXT_H_

#include "arena.h"
#include "ockham/ockham.h"

typedef struct {
    OkmArch arch;
    OkmOS os;

    uint8_t ptr_size;
    OkmType isize_type;
} OkmTargetInfo;

typedef struct OkmContext {
    OkmTargetInfo target;
    OkmArena arena;
} OkmContext;

#endif  // OCKHAM_LIB_CONTEXT_H_
