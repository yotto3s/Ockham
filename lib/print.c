#include <stdio.h>

#include "helpers.h"
#include "ir.h"
#include "ockham/ockham.h"

static const char* type_to_str(const OkmType type) {
    switch (type) {
        case OKM_TY_I8:
            return "i8";
        case OKM_TY_I16:
            return "i16";
        case OKM_TY_I32:
            return "i32";
        case OKM_TY_I64:
            return "i64";
        case OKM_TY_PTR:
            return "ptr";
        default:
            fprintf(stderr, "Unknown type\n");
            return "unknown";
    }
}

static const char* op_to_str(const OkmOp op) {
    switch (op) {
        case OKM_OP_CONST:
            return "const_int";

        case OKM_OP_ADD:
            return "add";
        case OKM_OP_SUB:
            return "sub";
        case OKM_OP_MUL:
            return "mul";
        case OKM_OP_SDIV:
            return "sdiv";
        case OKM_OP_UDIV:
            return "udiv";
        case OKM_OP_SREM:
            return "srem";
        case OKM_OP_UREM:
            return "urem";

        case OKM_OP_AND:
            return "and";
        case OKM_OP_OR:
            return "or";
        case OKM_OP_XOR:
            return "xor";
        case OKM_OP_SHL:
            return "shl";
        case OKM_OP_SHR:
            return "shr";

        case OKM_OP_EXTS:
            return "exts";
        case OKM_OP_EXTZ:
            return "extz";
        case OKM_OP_TRUNC:
            return "trunc";

        case OKM_OP_ALLOCA:
            return "alloca";
        case OKM_OP_LOAD:
            return "load";
        case OKM_OP_STORE:
            return "store";

        case OKM_OP_JMP:
            return "jmp";
        case OKM_OP_BRANCH:
            return "br";
        case OKM_OP_CALL:
            return "call";
        case OKM_OP_RET:
            return "ret";
        case OKM_OP_SYSCALL:
            return "syscall";
        default:
            fprintf(stderr, "Unknown op\n");
            return "unknown";
    }
}

static void okm_print_value(const OkmValue* const val, FILE* const fp) {
    switch (val->kind) {
        case OKM_VALUE_KIND_REG:
            fprintf(fp, "%%%u", val->as.reg.id);
            break;
        case OKM_VALUE_KIND_GLOBAL_SYMBOL:  // fall through
        case OKM_VALUE_KIND_FUNCTION_SYMBOL:
            fprintf(fp, "@%s", val->as.sym.symbol);
            break;
        default:
            fprintf(stderr, "Unknown value kind\n");
            break;
    }
}

static void okm_print_binary_op(const OkmInstr* const instr, FILE* const fp) {
    okm_print_value(instr->as.alu.dst, fp);
    fprintf(fp, " = %s ", op_to_str(instr->op));
    okm_print_value(instr->as.alu.lhs, fp);
    fprintf(fp, " ");
    okm_print_value(instr->as.alu.rhs, fp);
    fprintf(fp, " : %s\n", type_to_str(instr->as.alu.dst->type));
}

static void okm_print_unary_op(const OkmInstr* const instr, FILE* const fp) {
    okm_print_value(instr->as.alu.dst, fp);
    fprintf(fp, " = %s ", op_to_str(instr->op));
    okm_print_value(instr->as.alu.lhs, fp);
    fprintf(fp, " : %s\n", type_to_str(instr->as.alu.dst->type));
}

void okm_print_instr(const OkmInstr* const instr, FILE* const fp) {
    fprintf(fp, "    ");
    if (instr->op == OKM_OP_CONST) {
        okm_print_value(instr->as.imm.dst, fp);
        fprintf(fp, " = const_int %llu : %s\n", instr->as.imm.i,
                type_to_str(instr->as.imm.dst->type));
    } else if (is_binary_op(instr->op)) {
        okm_print_binary_op(instr, fp);
    } else if (is_unary_op(instr->op)) {
        okm_print_unary_op(instr, fp);
    } else if (instr->op == OKM_OP_ALLOCA) {
        okm_print_value(instr->as.mem.dst, fp);
        fprintf(fp, " = alloc %u\n", instr->as.mem.bytes);
    } else if (instr->op == OKM_OP_LOAD) {
        okm_print_value(instr->as.mem.dst, fp);
        fprintf(fp, " = load ");
        okm_print_value(instr->as.mem.ptr, fp);
        fprintf(fp, " : %s\n", type_to_str(instr->as.mem.dst->type));
    } else if (instr->op == OKM_OP_STORE) {
        fprintf(fp, "store ");
        okm_print_value(instr->as.mem.val, fp);
        fprintf(fp, ", ");
        okm_print_value(instr->as.mem.ptr, fp);
        fprintf(fp, " : %s\n", type_to_str(instr->as.mem.val->type));
    } else if (instr->op == OKM_OP_JMP) {
        fprintf(fp, "jmp ^block%u", instr->as.jmp.target->id);
        if (instr->as.jmp.arg_count > 0u) {
            fprintf(fp, "(");
            for (uint32_t i = 0u; i < instr->as.jmp.arg_count; ++i) {
                if (i > 0u) {
                    fprintf(fp, ", ");
                }
                okm_print_value(instr->as.jmp.args[i], fp);
            }
            fprintf(fp, ")");
        }
        fprintf(fp, "\n");
    } else if (instr->op == OKM_OP_BRANCH) {
        fprintf(fp, "br ");
        okm_print_value(instr->as.br.cond, fp);
        fprintf(fp, ", ^block%u", instr->as.br.target_true->id);
        if (instr->as.br.arg_count_true > 0u) {
            fprintf(fp, "(");
            for (uint32_t i = 0u; i < instr->as.br.arg_count_true; ++i) {
                if (i > 0u) {
                    fprintf(fp, ", ");
                }
                okm_print_value(instr->as.br.args_true[i], fp);
            }
            fprintf(fp, ")");
        }
        fprintf(fp, ", ^block%u", instr->as.br.target_false->id);
        if (instr->as.br.arg_count_false > 0u) {
            fprintf(fp, "(");
            for (uint32_t i = 0u; i < instr->as.br.arg_count_false; ++i) {
                if (i > 0u) {
                    fprintf(fp, ", ");
                }
                okm_print_value(instr->as.br.args_false[i], fp);
            }
            fprintf(fp, ")");
        }
        fprintf(fp, "\n");
    } else if (instr->op == OKM_OP_CALL) {
        if (instr->as.call.dst_count > 0u) {
            for (uint32_t i = 0u; i < instr->as.call.dst_count; ++i) {
                if (i > 0u) {
                    fprintf(fp, ", ");
                }
                okm_print_value(instr->as.call.dsts[i], fp);
            }
            fprintf(fp, " = ");
        }
        fprintf(fp, "call ");
        okm_print_value(instr->as.call.func, fp);
        fprintf(fp, "(");
        for (uint32_t i = 0u; i < instr->as.call.arg_count; ++i) {
            okm_print_value(instr->as.call.args[i], fp);
            if (i < instr->as.call.arg_count - 1u) {
                fprintf(fp, ", ");
            }
        }
        fprintf(fp, ")");
        for (uint32_t i = 0u; i < instr->as.call.dst_count; ++i) {
            if (i == 0u) {
                fprintf(fp, " : ");
            } else {
                fprintf(fp, ", ");
            }
            fprintf(fp, "%s", type_to_str(instr->as.call.dsts[i]->type));
        }
        fprintf(fp, "\n");
    } else if (instr->op == OKM_OP_RET) {
        fprintf(fp, "ret ");
        for (uint32_t i = 0u; i < instr->as.ret.value_count; ++i) {
            if (i > 0u) {
                fprintf(fp, ", ");
            }
            okm_print_value(instr->as.ret.values[i], fp);
        }
        fprintf(fp, "\n");
    } else if (instr->op == OKM_OP_SYSCALL) {
        if (instr->as.syscall.dst) {
            okm_print_value(instr->as.syscall.dst, fp);
            fprintf(fp, " = ");
        }
        fprintf(fp, "syscall ");
        okm_print_value(instr->as.syscall.sys_num, fp);
        fprintf(fp, "(");
        for (uint32_t i = 0u; i < instr->as.syscall.arg_count; ++i) {
            if (i > 0u) {
                fprintf(fp, ", ");
            }
            okm_print_value(instr->as.syscall.args[i], fp);
        }
        fprintf(fp, ")");
        if (instr->as.syscall.dst) {
            fprintf(fp, " : %s", type_to_str(instr->as.syscall.dst->type));
        }
        fprintf(fp, "\n");
    }
    fprintf(stderr, "Unknown op\n");
}

void okm_print_function(OkmFunction* const func, FILE* const fp) {
    fprintf(fp, "func @%s(", func->name);
    for (uint32_t i = 0u; i < func->param_count; ++i) {
        if (i > 0u) {
            fprintf(fp, ", ");
        }
        okm_print_value(func->params[i], fp);
        fprintf(fp, " : %s", type_to_str(func->params[i]->type));
    }
    fprintf(fp, ")");

    if (func->return_type_count > 0u) {
        fprintf(fp, " -> %s", type_to_str(func->return_types[0]));
        for (uint32_t i = 1u; i < func->return_type_count; ++i) {
            fprintf(fp, ", %s", type_to_str(func->return_types[i]));
        }
    }
    fprintf(fp, " {\n");

    for (const OkmBlock* block = func->block_head; block != NULL;
         block = block->next) {
        fprintf(fp, "^block%u", block->id);
        if (block->param_count > 0u) {
            fprintf(fp, "(");
            for (uint32_t i = 0u; i < block->param_count; ++i) {
                if (i > 0u) {
                    fprintf(fp, ", ");
                }
                okm_print_value(block->params[i], fp);
                fprintf(fp, " : %s", type_to_str(block->params[i]->type));
            }
            fprintf(fp, ")");
        }
        fprintf(fp, ":\n");
        for (const OkmInstr* instr = block->instr_head; instr != NULL;
             instr = instr->next) {
            okm_print_instr(instr, fp);
        }
    }
    fprintf(fp, "}\n");
}
