#ifndef OCKHAM_LIB_X86_64_EMIT_H_
#define OCKHAM_LIB_X86_64_EMIT_H_

#include <stdio.h>

typedef struct OkmFunction OkmFunction;
typedef struct OkmContext OkmContext;

void okm_lower_function_x86_64(const OkmContext* const ctx,
                               const OkmFunction* const func, FILE* const fp);
#endif  // OCKHAM_LIB_X86_64_EMIT_H_
