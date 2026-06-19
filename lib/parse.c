#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "arena.h"
#include "context.h"
#include "ir.h"
#include "ockham/ockham.h"

/* Reasonable upper bounds to prevent capacity-growth integer overflow. */
#define OKM_MAX_REG_ID UINT32_C(0xFFFFFF) /* 16 M registers per function */
#define OKM_MAX_BLOCK_ID UINT32_C(0xFFFF) /* 64 K blocks per function */

typedef enum {
    TOK_EOF,
    TOK_KEYWORD,
    TOK_REG,
    TOK_SYMBOL,
    TOK_BLOCK,
    TOK_TYPE,
    TOK_INT,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_LBRACE,
    TOK_RBRACE,
    TOK_COMMA,
    TOK_COLON,
    TOK_EQUAL,
    TOK_ARROW,
    TOK_ERROR
} TokenType;

typedef struct {
    TokenType type;
    const char* start;
    size_t length;
    uint64_t val_int;
    char* str_val;
} Token;

static Token next_token(OkmContext* ctx, const char** src) {
    const char* p = *src;
    while (*p && (*p == ' ' || *p == '\t' || *p == '\r' || *p == '\n')) {
        p++;
    }

    Token tok;
    tok.start = p;
    tok.length = 0;
    tok.val_int = 0;
    tok.str_val = NULL;

    switch (*p) {
        case '\0':
            tok.type = TOK_EOF;
            *src = p;
            return tok;
        case '(':
            tok.type = TOK_LPAREN;
            tok.length = 1;
            *src = p + 1;
            return tok;
        case ')':
            tok.type = TOK_RPAREN;
            tok.length = 1;
            *src = p + 1;
            return tok;
        case '{':
            tok.type = TOK_LBRACE;
            tok.length = 1;
            *src = p + 1;
            return tok;
        case '}':
            tok.type = TOK_RBRACE;
            tok.length = 1;
            *src = p + 1;
            return tok;
        case ',':
            tok.type = TOK_COMMA;
            tok.length = 1;
            *src = p + 1;
            return tok;
        case ':':
            tok.type = TOK_COLON;
            tok.length = 1;
            *src = p + 1;
            return tok;
        case '=':
            tok.type = TOK_EQUAL;
            tok.length = 1;
            *src = p + 1;
            return tok;
        case '-':
            if (p[1] == '>') {
                tok.type = TOK_ARROW;
                tok.length = 2;
                *src = p + 2;
                return tok;
            }
            break;
        case '%': {
            p++;
            if (*p < '0' || *p > '9') {
                tok.type = TOK_ERROR;
                return tok;
            }
            uint64_t val = 0;
            while (*p >= '0' && *p <= '9') {
                val = val * 10 + (*p - '0');
                p++;
            }
            if (val > OKM_MAX_REG_ID) {
                fprintf(stderr, "Error: register id %llu exceeds maximum %u\n",
                        (uint64_t)val, OKM_MAX_REG_ID);
                tok.type = TOK_ERROR;
                return tok;
            }
            tok.type = TOK_REG;
            tok.length = p - tok.start;
            tok.val_int = val;
            *src = p;
            return tok;
        }
        case '^': {
            if (strncmp(p, "^block", 6) != 0) {
                tok.type = TOK_ERROR;
                return tok;
            }
            p += 6;
            if (*p < '0' || *p > '9') {
                tok.type = TOK_ERROR;
                return tok;
            }
            uint64_t val = 0;
            while (*p >= '0' && *p <= '9') {
                val = val * 10 + (*p - '0');
                p++;
            }
            if (val > OKM_MAX_BLOCK_ID) {
                fprintf(stderr, "Error: block id %llu exceeds maximum %u\n",
                        (uint64_t)val, OKM_MAX_BLOCK_ID);
                tok.type = TOK_ERROR;
                return tok;
            }
            tok.type = TOK_BLOCK;
            tok.length = p - tok.start;
            tok.val_int = val;
            *src = p;
            return tok;
        }
        case '@': {
            p++;
            const char* start = p;
            while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') ||
                   (*p >= '0' && *p <= '9') || *p == '_') {
                p++;
            }
            size_t len = p - start;
            if (len == 0) {
                tok.type = TOK_ERROR;
                return tok;
            }
            char* sym = okm_arena_alloc(&ctx->arena, len + 1);
            memcpy(sym, start, len);
            sym[len] = '\0';
            tok.type = TOK_SYMBOL;
            tok.length = p - tok.start;
            tok.str_val = sym;
            *src = p;
            return tok;
        }
        default:
            break;
    }

    if (*p >= '0' && *p <= '9') {
        uint64_t val = 0;
        while (*p >= '0' && *p <= '9') {
            val = val * 10 + (*p - '0');
            p++;
        }
        tok.type = TOK_INT;
        tok.length = p - tok.start;
        tok.val_int = val;
        *src = p;
        return tok;
    }

    if ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || *p == '_') {
        const char* start = p;
        while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') ||
               (*p >= '0' && *p <= '9') || *p == '_') {
            p++;
        }
        size_t len = p - start;
        tok.length = len;
        *src = p;

        if (len == 2 && strncmp(start, "i8", 2) == 0) {
            tok.type = TOK_TYPE;
            tok.val_int = OKM_TY_I8;
            return tok;
        }
        if (len == 3 && strncmp(start, "i16", 3) == 0) {
            tok.type = TOK_TYPE;
            tok.val_int = OKM_TY_I16;
            return tok;
        }
        if (len == 3 && strncmp(start, "i32", 3) == 0) {
            tok.type = TOK_TYPE;
            tok.val_int = OKM_TY_I32;
            return tok;
        }
        if (len == 3 && strncmp(start, "i64", 3) == 0) {
            tok.type = TOK_TYPE;
            tok.val_int = OKM_TY_I64;
            return tok;
        }
        if (len == 3 && strncmp(start, "ptr", 3) == 0) {
            tok.type = TOK_TYPE;
            tok.val_int = OKM_TY_PTR;
            return tok;
        }

        char* name = okm_arena_alloc(&ctx->arena, len + 1);
        memcpy(name, start, len);
        name[len] = '\0';
        tok.type = TOK_KEYWORD;
        tok.str_val = name;
        return tok;
    }

    tok.type = TOK_ERROR;
    return tok;
}

static Token peek_token(OkmContext* ctx, const char* src) {
    return next_token(ctx, &src);
}

static bool match_token(OkmContext* ctx, const char** src, TokenType type,
                        Token* tok_out) {
    const char* save = *src;
    Token t = next_token(ctx, src);
    if (t.type == type) {
        if (tok_out) *tok_out = t;
        return true;
    }
    *src = save;
    return false;
}

static bool match_keyword(OkmContext* ctx, const char** src, const char* kw) {
    const char* save = *src;
    Token t = next_token(ctx, src);
    if (t.type == TOK_KEYWORD && strcmp(t.str_val, kw) == 0) {
        return true;
    }
    *src = save;
    return false;
}

static OkmValue* get_or_create_reg(OkmContext* ctx, uint32_t reg_id,
                                   OkmValue*** regs_arr, uint32_t* regs_cap) {
    if (reg_id >= *regs_cap) {
        uint32_t new_cap = *regs_cap * 2;
        if (new_cap <= reg_id) {
            if (reg_id > UINT32_MAX - 64) {
                fprintf(stderr,
                        "Error: register id %u would overflow capacity\n",
                        reg_id);
                return NULL;
            }
            new_cap = reg_id + 64;
        }
        OkmValue** new_arr =
            okm_arena_alloc(&ctx->arena, sizeof(OkmValue*) * new_cap);
        memset(new_arr, 0, sizeof(OkmValue*) * new_cap);
        if (*regs_arr) {
            memcpy(new_arr, *regs_arr, sizeof(OkmValue*) * (*regs_cap));
        }
        *regs_arr = new_arr;
        *regs_cap = new_cap;
    }
    if ((*regs_arr)[reg_id] == NULL) {
        OkmValue* val = okm_arena_alloc(&ctx->arena, sizeof(OkmValue));
        val->kind = OKM_VALUE_KIND_REG;
        val->type = OKM_TY_I32;
        val->as.reg.id = reg_id;
        val->as.reg.def = NULL;
        (*regs_arr)[reg_id] = val;
    }
    return (*regs_arr)[reg_id];
}

static OkmBlock* parse_get_or_create_block(OkmContext* ctx, OkmFunction* func,
                                           uint32_t block_id,
                                           OkmBlock*** blocks_arr,
                                           uint32_t* blocks_cap) {
    if (block_id >= *blocks_cap) {
        uint32_t new_cap = *blocks_cap * 2;
        if (new_cap <= block_id) {
            if (block_id > UINT32_MAX - 16) {
                fprintf(stderr, "Error: block id %u would overflow capacity\n",
                        block_id);
                return NULL;
            }
            new_cap = block_id + 16;
        }
        OkmBlock** new_arr =
            okm_arena_alloc(&ctx->arena, sizeof(OkmBlock*) * new_cap);
        memset(new_arr, 0, sizeof(OkmBlock*) * new_cap);
        if (*blocks_arr) {
            memcpy(new_arr, *blocks_arr, sizeof(OkmBlock*) * (*blocks_cap));
        }
        *blocks_arr = new_arr;
        *blocks_cap = new_cap;
    }
    if ((*blocks_arr)[block_id] == NULL) {
        OkmBlock* block = okm_arena_alloc(&ctx->arena, sizeof(OkmBlock));
        block->id = block_id;
        block->params = NULL;
        block->param_count = 0u;
        block->function = func;
        block->instr_head = NULL;
        block->instr_tail = NULL;
        block->prev = NULL;
        block->next = NULL;
        (*blocks_arr)[block_id] = block;
    }
    return (*blocks_arr)[block_id];
}

static bool parse_instruction(OkmContext* ctx, const char** src,
                              OkmBlock* block, OkmValue*** regs_arr,
                              uint32_t* regs_cap, OkmBlock*** blocks_arr,
                              uint32_t* blocks_cap) {
    uint32_t dst_count = 0;
    uint32_t dst_cap = 4;
    Token* dst_toks = okm_arena_alloc(&ctx->arena, sizeof(Token) * dst_cap);

    const char* temp_src = *src;
    bool has_equal = false;
    while (true) {
        Token tok = next_token(ctx, &temp_src);
        if (tok.type == TOK_REG) {
            if (dst_count >= dst_cap) {
                dst_cap *= 2;
                Token* new_toks =
                    okm_arena_alloc(&ctx->arena, sizeof(Token) * dst_cap);
                memcpy(new_toks, dst_toks, sizeof(Token) * dst_count);
                dst_toks = new_toks;
            }
            dst_toks[dst_count++] = tok;

            Token next = next_token(ctx, &temp_src);
            if (next.type == TOK_EQUAL) {
                has_equal = true;
                break;
            }
            if (next.type == TOK_COMMA) {
                continue;
            }
            break;
        }
        break;
    }

    if (has_equal) {
        *src = temp_src;
    } else {
        dst_count = 0;
    }

    Token tok_op;
    if (!match_token(ctx, src, TOK_KEYWORD, &tok_op)) {
        return false;
    }

#define GET_REG(id) get_or_create_reg(ctx, id, regs_arr, regs_cap)
#define GET_BLOCK(id) \
    parse_get_or_create_block(ctx, block->function, id, blocks_arr, blocks_cap)

    const char* op_str = tok_op.str_val;

    OkmInstr* instr = okm_arena_alloc(&ctx->arena, sizeof(OkmInstr));
    instr->next = NULL;
    instr->prev = block->instr_tail;
    if (block->instr_tail) {
        block->instr_tail->next = instr;
    } else {
        block->instr_head = instr;
    }
    block->instr_tail = instr;

    if (strcmp(op_str, "const_int") == 0) {
        if (dst_count != 1) return false;
        instr->op = OKM_OP_CONST;

        Token tok_val;
        if (!match_token(ctx, src, TOK_INT, &tok_val)) return false;

        if (!match_token(ctx, src, TOK_COLON, NULL)) return false;
        Token tok_type;
        if (!match_token(ctx, src, TOK_TYPE, &tok_type)) return false;

        OkmValue* dst = GET_REG(dst_toks[0].val_int);
        dst->type = (OkmType)tok_type.val_int;
        dst->as.reg.def = instr;

        instr->as.imm.dst = dst;
        instr->as.imm.i = tok_val.val_int;
        return true;
    }

    OkmOp alu_op = OKM_OP_ADD;
    bool is_alu = false;
    bool is_unary = false;
    if (strcmp(op_str, "add") == 0) {
        alu_op = OKM_OP_ADD;
        is_alu = true;
    } else if (strcmp(op_str, "sub") == 0) {
        alu_op = OKM_OP_SUB;
        is_alu = true;
    } else if (strcmp(op_str, "mul") == 0) {
        alu_op = OKM_OP_MUL;
        is_alu = true;
    } else if (strcmp(op_str, "sdiv") == 0) {
        alu_op = OKM_OP_SDIV;
        is_alu = true;
    } else if (strcmp(op_str, "udiv") == 0) {
        alu_op = OKM_OP_UDIV;
        is_alu = true;
    } else if (strcmp(op_str, "srem") == 0) {
        alu_op = OKM_OP_SREM;
        is_alu = true;
    } else if (strcmp(op_str, "urem") == 0) {
        alu_op = OKM_OP_UREM;
        is_alu = true;
    } else if (strcmp(op_str, "and") == 0) {
        alu_op = OKM_OP_AND;
        is_alu = true;
    } else if (strcmp(op_str, "or") == 0) {
        alu_op = OKM_OP_OR;
        is_alu = true;
    } else if (strcmp(op_str, "xor") == 0) {
        alu_op = OKM_OP_XOR;
        is_alu = true;
    } else if (strcmp(op_str, "shl") == 0) {
        alu_op = OKM_OP_SHL;
        is_alu = true;
    } else if (strcmp(op_str, "shr") == 0) {
        alu_op = OKM_OP_SHR;
        is_alu = true;
    } else if (strcmp(op_str, "exts") == 0) {
        alu_op = OKM_OP_EXTS;
        is_unary = true;
    } else if (strcmp(op_str, "extz") == 0) {
        alu_op = OKM_OP_EXTZ;
        is_unary = true;
    } else if (strcmp(op_str, "trunc") == 0) {
        alu_op = OKM_OP_TRUNC;
        is_unary = true;
    }

    if (is_alu) {
        if (dst_count != 1) return false;
        instr->op = alu_op;

        Token tok_lhs, tok_rhs;
        if (!match_token(ctx, src, TOK_REG, &tok_lhs)) return false;
        if (!match_token(ctx, src, TOK_REG, &tok_rhs)) return false;

        if (!match_token(ctx, src, TOK_COLON, NULL)) return false;
        Token tok_type;
        if (!match_token(ctx, src, TOK_TYPE, &tok_type)) return false;

        OkmValue* dst = GET_REG(dst_toks[0].val_int);
        dst->type = (OkmType)tok_type.val_int;
        dst->as.reg.def = instr;

        instr->as.alu.dst = dst;
        instr->as.alu.lhs = GET_REG(tok_lhs.val_int);
        instr->as.alu.rhs = GET_REG(tok_rhs.val_int);
        return true;
    }

    if (is_unary) {
        if (dst_count != 1) return false;
        instr->op = alu_op;

        Token tok_lhs;
        if (!match_token(ctx, src, TOK_REG, &tok_lhs)) return false;

        if (!match_token(ctx, src, TOK_COLON, NULL)) return false;
        Token tok_type;
        if (!match_token(ctx, src, TOK_TYPE, &tok_type)) return false;

        OkmValue* dst = GET_REG(dst_toks[0].val_int);
        dst->type = (OkmType)tok_type.val_int;
        dst->as.reg.def = instr;

        instr->as.alu.dst = dst;
        instr->as.alu.lhs = GET_REG(tok_lhs.val_int);
        instr->as.alu.rhs = NULL;
        return true;
    }

    if (strcmp(op_str, "alloc") == 0) {
        if (dst_count != 1) return false;
        instr->op = OKM_OP_ALLOCA;

        Token tok_bytes;
        if (!match_token(ctx, src, TOK_INT, &tok_bytes)) return false;

        OkmValue* dst = GET_REG(dst_toks[0].val_int);
        dst->type = OKM_TY_PTR;
        dst->as.reg.def = instr;

        instr->as.mem.dst = dst;
        if (tok_bytes.val_int > UINT32_MAX) {
            fprintf(stderr, "Error: alloc byte count %llu exceeds maximum %u\n",
                    (uint64_t)tok_bytes.val_int, UINT32_MAX);
            return false;
        }
        instr->as.mem.bytes = (uint32_t)tok_bytes.val_int;
        instr->as.mem.ptr = NULL;
        instr->as.mem.val = NULL;
        return true;
    }

    if (strcmp(op_str, "load") == 0) {
        if (dst_count != 1) return false;
        instr->op = OKM_OP_LOAD;

        Token tok_ptr;
        if (!match_token(ctx, src, TOK_REG, &tok_ptr)) return false;

        if (!match_token(ctx, src, TOK_COLON, NULL)) return false;
        Token tok_type;
        if (!match_token(ctx, src, TOK_TYPE, &tok_type)) return false;

        OkmValue* dst = GET_REG(dst_toks[0].val_int);
        dst->type = (OkmType)tok_type.val_int;
        dst->as.reg.def = instr;

        instr->as.mem.dst = dst;
        instr->as.mem.ptr = GET_REG(tok_ptr.val_int);
        instr->as.mem.val = NULL;
        instr->as.mem.bytes = 0;
        return true;
    }

    if (strcmp(op_str, "store") == 0) {
        if (dst_count != 0) return false;
        instr->op = OKM_OP_STORE;

        Token tok_val, tok_ptr;
        if (!match_token(ctx, src, TOK_REG, &tok_val)) return false;
        if (!match_token(ctx, src, TOK_COMMA, NULL)) return false;
        if (!match_token(ctx, src, TOK_REG, &tok_ptr)) return false;

        if (!match_token(ctx, src, TOK_COLON, NULL)) return false;
        Token tok_type;
        if (!match_token(ctx, src, TOK_TYPE, &tok_type)) return false;

        OkmValue* val = GET_REG(tok_val.val_int);
        val->type = (OkmType)tok_type.val_int;

        instr->as.mem.dst = NULL;
        instr->as.mem.ptr = GET_REG(tok_ptr.val_int);
        instr->as.mem.val = val;
        instr->as.mem.bytes = 0;
        return true;
    }

    if (strcmp(op_str, "jmp") == 0) {
        if (dst_count != 0) return false;
        instr->op = OKM_OP_JMP;

        Token tok_block;
        if (!match_token(ctx, src, TOK_BLOCK, &tok_block)) return false;

        OkmBlock* target = GET_BLOCK(tok_block.val_int);
        uint32_t arg_count = 0;
        OkmValue** args = NULL;

        if (match_token(ctx, src, TOK_LPAREN, NULL)) {
            uint32_t arg_cap = 4;
            args = okm_arena_alloc(&ctx->arena, sizeof(OkmValue*) * arg_cap);
            while (true) {
                Token tok_arg;
                if (match_token(ctx, src, TOK_REG, &tok_arg)) {
                    if (arg_count >= arg_cap) {
                        arg_cap *= 2;
                        OkmValue** new_args = okm_arena_alloc(
                            &ctx->arena, sizeof(OkmValue*) * arg_cap);
                        memcpy(new_args, args, sizeof(OkmValue*) * arg_count);
                        args = new_args;
                    }
                    args[arg_count++] = GET_REG(tok_arg.val_int);
                    if (match_token(ctx, src, TOK_COMMA, NULL)) {
                        continue;
                    }
                }
                break;
            }
            if (!match_token(ctx, src, TOK_RPAREN, NULL)) return false;
        }

        instr->as.jmp.target = target;
        instr->as.jmp.args = args;
        instr->as.jmp.arg_count = arg_count;
        return true;
    }

    if (strcmp(op_str, "br") == 0) {
        if (dst_count != 0) return false;
        instr->op = OKM_OP_BRANCH;

        Token tok_cond, tok_true, tok_false;
        if (!match_token(ctx, src, TOK_REG, &tok_cond)) return false;
        if (!match_token(ctx, src, TOK_COMMA, NULL)) return false;

        if (!match_token(ctx, src, TOK_BLOCK, &tok_true)) return false;
        uint32_t arg_count_true = 0;
        OkmValue** args_true = NULL;
        if (match_token(ctx, src, TOK_LPAREN, NULL)) {
            uint32_t arg_cap = 4;
            args_true =
                okm_arena_alloc(&ctx->arena, sizeof(OkmValue*) * arg_cap);
            while (true) {
                Token tok_arg;
                if (match_token(ctx, src, TOK_REG, &tok_arg)) {
                    if (arg_count_true >= arg_cap) {
                        arg_cap *= 2;
                        OkmValue** new_args = okm_arena_alloc(
                            &ctx->arena, sizeof(OkmValue*) * arg_cap);
                        memcpy(new_args, args_true,
                               sizeof(OkmValue*) * arg_count_true);
                        args_true = new_args;
                    }
                    args_true[arg_count_true++] = GET_REG(tok_arg.val_int);
                    if (match_token(ctx, src, TOK_COMMA, NULL)) {
                        continue;
                    }
                }
                break;
            }
            if (!match_token(ctx, src, TOK_RPAREN, NULL)) return false;
        }

        if (!match_token(ctx, src, TOK_COMMA, NULL)) return false;

        if (!match_token(ctx, src, TOK_BLOCK, &tok_false)) return false;
        uint32_t arg_count_false = 0;
        OkmValue** args_false = NULL;
        if (match_token(ctx, src, TOK_LPAREN, NULL)) {
            uint32_t arg_cap = 4;
            args_false =
                okm_arena_alloc(&ctx->arena, sizeof(OkmValue*) * arg_cap);
            while (true) {
                Token tok_arg;
                if (match_token(ctx, src, TOK_REG, &tok_arg)) {
                    if (arg_count_false >= arg_cap) {
                        arg_cap *= 2;
                        OkmValue** new_args = okm_arena_alloc(
                            &ctx->arena, sizeof(OkmValue*) * arg_cap);
                        memcpy(new_args, args_false,
                               sizeof(OkmValue*) * arg_count_false);
                        args_false = new_args;
                    }
                    args_false[arg_count_false++] = GET_REG(tok_arg.val_int);
                    if (match_token(ctx, src, TOK_COMMA, NULL)) {
                        continue;
                    }
                }
                break;
            }
            if (!match_token(ctx, src, TOK_RPAREN, NULL)) return false;
        }

        instr->as.br.cond = GET_REG(tok_cond.val_int);
        instr->as.br.target_true = GET_BLOCK(tok_true.val_int);
        instr->as.br.args_true = args_true;
        instr->as.br.arg_count_true = arg_count_true;
        instr->as.br.target_false = GET_BLOCK(tok_false.val_int);
        instr->as.br.args_false = args_false;
        instr->as.br.arg_count_false = arg_count_false;
        return true;
    }

    if (strcmp(op_str, "ret") == 0) {
        if (dst_count != 0) return false;
        instr->op = OKM_OP_RET;

        uint32_t val_count = 0;
        uint32_t val_cap = 4;
        OkmValue** values =
            okm_arena_alloc(&ctx->arena, sizeof(OkmValue*) * val_cap);

        while (true) {
            Token tok_val;
            if (match_token(ctx, src, TOK_REG, &tok_val)) {
                if (val_count >= val_cap) {
                    val_cap *= 2;
                    OkmValue** new_values = okm_arena_alloc(
                        &ctx->arena, sizeof(OkmValue*) * val_cap);
                    memcpy(new_values, values, sizeof(OkmValue*) * val_count);
                    values = new_values;
                }
                values[val_count++] = GET_REG(tok_val.val_int);
                if (match_token(ctx, src, TOK_COMMA, NULL)) {
                    continue;
                }
            }
            break;
        }

        instr->as.ret.values = values;
        instr->as.ret.value_count = val_count;
        return true;
    }

    if (strcmp(op_str, "call") == 0) {
        instr->op = OKM_OP_CALL;

        Token tok_callee;
        OkmValue* callee = NULL;
        if (match_token(ctx, src, TOK_SYMBOL, &tok_callee)) {
            callee = okm_arena_alloc(&ctx->arena, sizeof(OkmValue));
            callee->kind = OKM_VALUE_KIND_FUNCTION_SYMBOL;
            callee->type = OKM_TY_PTR;
            callee->as.sym.symbol = tok_callee.str_val;
        } else if (match_token(ctx, src, TOK_REG, &tok_callee)) {
            callee = GET_REG(tok_callee.val_int);
        } else {
            return false;
        }

        if (!match_token(ctx, src, TOK_LPAREN, NULL)) return false;

        uint32_t arg_count = 0;
        uint32_t arg_cap = 4;
        OkmValue** args =
            okm_arena_alloc(&ctx->arena, sizeof(OkmValue*) * arg_cap);
        while (true) {
            Token tok_arg;
            if (match_token(ctx, src, TOK_REG, &tok_arg)) {
                if (arg_count >= arg_cap) {
                    arg_cap *= 2;
                    OkmValue** new_args = okm_arena_alloc(
                        &ctx->arena, sizeof(OkmValue*) * arg_cap);
                    memcpy(new_args, args, sizeof(OkmValue*) * arg_count);
                    args = new_args;
                }
                args[arg_count++] = GET_REG(tok_arg.val_int);
                if (match_token(ctx, src, TOK_COMMA, NULL)) {
                    continue;
                }
            }
            break;
        }
        if (!match_token(ctx, src, TOK_RPAREN, NULL)) return false;

        if (dst_count > 0) {
            if (!match_token(ctx, src, TOK_COLON, NULL)) return false;
            OkmValue** dsts =
                okm_arena_alloc(&ctx->arena, sizeof(OkmValue*) * dst_count);
            for (uint32_t i = 0; i < dst_count; ++i) {
                Token tok_type;
                if (!match_token(ctx, src, TOK_TYPE, &tok_type)) return false;
                OkmValue* dst = GET_REG(dst_toks[i].val_int);
                dst->type = (OkmType)tok_type.val_int;
                dst->as.reg.def = instr;
                dsts[i] = dst;
                if (i < dst_count - 1) {
                    if (!match_token(ctx, src, TOK_COMMA, NULL)) return false;
                }
            }
            instr->as.call.dsts = dsts;
            instr->as.call.dst_count = dst_count;
        } else {
            instr->as.call.dsts = NULL;
            instr->as.call.dst_count = 0;
        }

        instr->as.call.func = callee;
        instr->as.call.args = args;
        instr->as.call.arg_count = arg_count;
        return true;
    }

    if (strcmp(op_str, "syscall") == 0) {
        if (dst_count > 1) return false;
        instr->op = OKM_OP_SYSCALL;

        Token tok_sys;
        if (!match_token(ctx, src, TOK_REG, &tok_sys)) return false;

        if (!match_token(ctx, src, TOK_LPAREN, NULL)) return false;

        uint32_t arg_count = 0;
        OkmValue* args[6];
        while (true) {
            Token tok_arg;
            if (match_token(ctx, src, TOK_REG, &tok_arg)) {
                if (arg_count >= 6) return false;
                args[arg_count++] = GET_REG(tok_arg.val_int);
                if (match_token(ctx, src, TOK_COMMA, NULL)) {
                    continue;
                }
            }
            break;
        }
        if (!match_token(ctx, src, TOK_RPAREN, NULL)) return false;

        OkmValue* dst = NULL;
        if (dst_count == 1) {
            if (!match_token(ctx, src, TOK_COLON, NULL)) return false;
            Token tok_type;
            if (!match_token(ctx, src, TOK_TYPE, &tok_type)) return false;
            dst = GET_REG(dst_toks[0].val_int);
            dst->type = (OkmType)tok_type.val_int;
            dst->as.reg.def = instr;
        }

        instr->as.syscall.dst = dst;
        instr->as.syscall.sys_num = GET_REG(tok_sys.val_int);
        instr->as.syscall.arg_count = arg_count;
        for (uint32_t i = 0; i < arg_count; ++i) {
            instr->as.syscall.args[i] = args[i];
        }
        return true;
    }

    return false;
}

OkmFunction* okm_parse_function(OkmContext* const ctx,
                                const char* const input) {
    const char* src = input;

    if (!match_keyword(ctx, &src, "func")) {
        return NULL;
    }

    Token tok_name;
    if (!match_token(ctx, &src, TOK_SYMBOL, &tok_name)) {
        return NULL;
    }

    if (!match_token(ctx, &src, TOK_LPAREN, NULL)) {
        return NULL;
    }

    uint32_t param_count = 0;
    uint32_t param_cap = 4;
    OkmValue** params =
        okm_arena_alloc(&ctx->arena, sizeof(OkmValue*) * param_cap);

    OkmValue** regs_arr = NULL;
    uint32_t regs_cap = 0;
    OkmBlock** blocks_arr = NULL;
    uint32_t blocks_cap = 0;

    while (true) {
        Token tok_reg;
        if (match_token(ctx, &src, TOK_REG, &tok_reg)) {
            if (!match_token(ctx, &src, TOK_COLON, NULL)) return NULL;
            Token tok_type;
            if (!match_token(ctx, &src, TOK_TYPE, &tok_type)) return NULL;

            OkmValue* val =
                get_or_create_reg(ctx, tok_reg.val_int, &regs_arr, &regs_cap);
            val->type = (OkmType)tok_type.val_int;

            if (param_count >= param_cap) {
                param_cap *= 2;
                OkmValue** new_params =
                    okm_arena_alloc(&ctx->arena, sizeof(OkmValue*) * param_cap);
                memcpy(new_params, params, sizeof(OkmValue*) * param_count);
                params = new_params;
            }
            params[param_count++] = val;

            if (match_token(ctx, &src, TOK_COMMA, NULL)) {
                continue;
            }
        }
        break;
    }

    if (!match_token(ctx, &src, TOK_RPAREN, NULL)) {
        return NULL;
    }

    uint32_t ret_count = 0;
    uint32_t ret_cap = 4;
    OkmType* ret_types =
        okm_arena_alloc(&ctx->arena, sizeof(OkmType) * ret_cap);

    if (match_token(ctx, &src, TOK_ARROW, NULL)) {
        while (true) {
            Token tok_type;
            if (match_token(ctx, &src, TOK_TYPE, &tok_type)) {
                if (ret_count >= ret_cap) {
                    ret_cap *= 2;
                    OkmType* new_ret_types =
                        okm_arena_alloc(&ctx->arena, sizeof(OkmType) * ret_cap);
                    memcpy(new_ret_types, ret_types,
                           sizeof(OkmType) * ret_count);
                    ret_types = new_ret_types;
                }
                ret_types[ret_count++] = (OkmType)tok_type.val_int;

                if (match_token(ctx, &src, TOK_COMMA, NULL)) {
                    continue;
                }
            }
            break;
        }
        if (ret_count == 0) return NULL;
    }

    OkmFunction* func =
        okm_new_function(ctx, tok_name.str_val, ret_types, ret_count);
    func->params = params;
    func->param_count = param_count;

    if (!match_token(ctx, &src, TOK_LBRACE, NULL)) {
        return NULL;
    }

    OkmBlock* current_block = NULL;

    while (true) {
        Token peek = peek_token(ctx, src);
        if (peek.type == TOK_RBRACE) {
            match_token(ctx, &src, TOK_RBRACE, NULL);
            break;
        }
        if (peek.type == TOK_EOF) {
            return NULL;
        }

        Token tok_block;
        if (match_token(ctx, &src, TOK_BLOCK, &tok_block)) {
            current_block = parse_get_or_create_block(
                ctx, func, tok_block.val_int, &blocks_arr, &blocks_cap);

            if (match_token(ctx, &src, TOK_LPAREN, NULL)) {
                uint32_t bparam_count = 0;
                uint32_t bparam_cap = 4;
                OkmValue** bparams = okm_arena_alloc(
                    &ctx->arena, sizeof(OkmValue*) * bparam_cap);

                while (true) {
                    Token tok_reg;
                    if (match_token(ctx, &src, TOK_REG, &tok_reg)) {
                        if (!match_token(ctx, &src, TOK_COLON, NULL))
                            return NULL;
                        Token tok_type;
                        if (!match_token(ctx, &src, TOK_TYPE, &tok_type))
                            return NULL;

                        OkmValue* val = get_or_create_reg(ctx, tok_reg.val_int,
                                                          &regs_arr, &regs_cap);
                        val->type = (OkmType)tok_type.val_int;

                        if (bparam_count >= bparam_cap) {
                            bparam_cap *= 2;
                            OkmValue** new_params = okm_arena_alloc(
                                &ctx->arena, sizeof(OkmValue*) * bparam_cap);
                            memcpy(new_params, bparams,
                                   sizeof(OkmValue*) * bparam_count);
                            bparams = new_params;
                        }
                        bparams[bparam_count++] = val;

                        if (match_token(ctx, &src, TOK_COMMA, NULL)) {
                            continue;
                        }
                    }
                    break;
                }

                if (!match_token(ctx, &src, TOK_RPAREN, NULL)) {
                    return NULL;
                }

                current_block->params = bparams;
                current_block->param_count = bparam_count;
            }

            if (!match_token(ctx, &src, TOK_COLON, NULL)) {
                return NULL;
            }

            if (current_block->prev != NULL ||
                func->block_head == current_block) {
                fprintf(stderr, "Error: duplicate definition of ^block%u\n",
                        current_block->id);
                return NULL;
            }

            if (func->block_tail != current_block) {
                current_block->prev = func->block_tail;
                if (func->block_tail) {
                    func->block_tail->next = current_block;
                } else {
                    func->block_head = current_block;
                }
                func->block_tail = current_block;
            }
            continue;
        }

        if (current_block == NULL) {
            return NULL;
        }

        if (!parse_instruction(ctx, &src, current_block, &regs_arr, &regs_cap,
                               &blocks_arr, &blocks_cap)) {
            return NULL;
        }
    }

    uint32_t max_reg_id = 0;
    for (uint32_t i = 0; i < regs_cap; ++i) {
        if (regs_arr[i] != NULL) {
            if (regs_arr[i]->as.reg.id >= max_reg_id) {
                max_reg_id = regs_arr[i]->as.reg.id + 1;
            }
        }
    }
    func->next_val_id = max_reg_id;
    uint32_t max_block_id = 0;
    for (uint32_t i = 0; i < blocks_cap; ++i) {
        if (blocks_arr[i] != NULL) {
            if (blocks_arr[i]->id >= max_block_id) {
                max_block_id = blocks_arr[i]->id + 1;
            }
        }
    }
    func->next_block_id = max_block_id;

    return func;
}
