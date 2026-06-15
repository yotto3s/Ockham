#include "arena.h"

#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define ARENA_CHUNK_SIZE (256u * 1024u)

void okm_arena_chunk_init(OkmArenaChunk* const chunk) {
    assert(chunk);
    chunk->buffer = (uint8_t*)malloc(ARENA_CHUNK_SIZE);
    if (!chunk->buffer) {
        fprintf(stderr, "Error: Failed to allocate chunk->buffer\n");
        exit(1);
    }
    chunk->size = ARENA_CHUNK_SIZE;
    chunk->offset = 0u;
    chunk->next = NULL;
}

void okm_arena_init(OkmArena* const arena) {
    assert(arena);
    arena->head = (OkmArenaChunk*)malloc(sizeof(OkmArenaChunk));
    if (!arena->head) {
        fprintf(stderr, "Error: Failed to allocate arena->head\n");
        exit(1);
    }
    okm_arena_chunk_init(arena->head);
    arena->active = arena->head;
}

void* okm_arena_alloc(OkmArena* const arena, const size_t size) {
    assert(arena);
    assert(size <= ARENA_CHUNK_SIZE);

    const size_t align = 8u;
    OkmArenaChunk* current_chunk = arena->active;
    size_t current_offset = current_chunk->offset;
    size_t padding = (align - (current_offset & (align - 1u))) & (align - 1u);
    size_t next_offset = current_offset + padding + size;

    if (next_offset > current_chunk->size) {
        OkmArenaChunk* next_chunk = current_chunk->next;
        if (!next_chunk) {
            next_chunk = (OkmArenaChunk*)malloc(sizeof(OkmArenaChunk));
            if (!next_chunk) {
                fprintf(stderr, "Error: Failed to allocate next_chunk\n");
                exit(1);
            }
            okm_arena_chunk_init(next_chunk);
            current_chunk->next = next_chunk;
            arena->active = next_chunk;
        }

        current_offset = 0u;
        padding = 0u;
        next_offset = size;
    }

    arena->active->offset = next_offset;
    return &arena->active->buffer[current_offset + padding];
}

void okm_arena_reset(OkmArena* const arena) {
    assert(arena);
    OkmArenaChunk* curr = arena->head;
    while (curr) {
        curr->offset = 0u;
        curr = curr->next;
    }

    arena->active = arena->head;
}

void okm_arena_destroy(OkmArena* const arena) {
    assert(arena);

    OkmArenaChunk* curr = arena->head;
    while (curr) {
        OkmArenaChunk* next = curr->next;

        free(curr->buffer);
        free(curr);

        curr = next;
    }

    arena->head = NULL;
    arena->active = NULL;
}
