#include "context.h"

#include <stdlib.h>

#include "arena.h"
#include "ockham/ockham.h"

OkmContext* okm_new_context(const OkmArch arch, const OkmOS os) {
    OkmContext* ctx = (OkmContext*)malloc(sizeof(OkmContext));
    ctx->target.arch = arch;
    ctx->target.os = os;
    switch (arch) {
        /* 64 bit */
        case OKM_ARCH_X86_64:  /* fall through */
        case OKM_ARCH_AARCH64: /* fall through */
            ctx->target.ptr_size = 8u;
            ctx->target.isize_type = OKM_TY_I64;
            break;
        default:
            ctx->target.ptr_size = 8u;
            ctx->target.isize_type = OKM_TY_I64;
            break;
    }

    okm_arena_init(&ctx->arena);

    return ctx;
}

void okm_destroy_context(OkmContext* const ctx) {
    okm_arena_destroy(&ctx->arena);
    free(ctx);
}
