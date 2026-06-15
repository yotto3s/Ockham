#ifndef OCKHAM_LIB_ARENA_H_
#define OCKHAM_LIB_ARENA_H_

#include <stddef.h>
#include <stdint.h>

typedef struct OkmArenaChunk {
    uint8_t* buffer;
    size_t size;
    size_t offset;
    struct OkmArenaChunk* next;
} OkmArenaChunk;

typedef struct OkmArena {
    OkmArenaChunk* head;
    OkmArenaChunk* active;
} OkmArena;

void okm_arena_init(OkmArena* const arena);
void* okm_arena_alloc(OkmArena* const arena, const size_t size);
void okm_arena_reset(OkmArena* const arena);
void okm_arena_destroy(OkmArena* const arena);

#endif  // OCKHAM_LIB_ARENA_H_
