#include "arena.h"

#include <string.h>
#include <unity.h>

void setUp(void) {}
void tearDown(void) {}

void test_ArenaInit(void) {
    OkmArena arena;
    okm_arena_init(&arena);

    TEST_ASSERT_NOT_NULL(arena.head);
    TEST_ASSERT_EQUAL_PTR(arena.head, arena.active);
    TEST_ASSERT_EQUAL_size_t(0u, arena.head->offset);
    TEST_ASSERT_NOT_NULL(arena.head->buffer);

    okm_arena_destroy(&arena);
}

void test_ArenaAllocBasic(void) {
    OkmArena arena;
    okm_arena_init(&arena);

    void* ptr = okm_arena_alloc(&arena, 64);
    TEST_ASSERT_NOT_NULL(ptr);

    okm_arena_destroy(&arena);
}

void test_ArenaAllocAlignment(void) {
    OkmArena arena;
    okm_arena_init(&arena);

    const size_t sizes[] = {1, 3, 7, 8, 9, 16, 31, 64};
    for (size_t i = 0; i < sizeof(sizes) / sizeof(sizes[0]); ++i) {
        void* ptr = okm_arena_alloc(&arena, sizes[i]);
        TEST_ASSERT_NOT_NULL(ptr);
        TEST_ASSERT_EQUAL_size_t(0u, (size_t)ptr % 8u);
    }

    okm_arena_destroy(&arena);
}

void test_ArenaAllocNonOverlap(void) {
    OkmArena arena;
    okm_arena_init(&arena);

    const size_t count = 16;
    const size_t block = 32;
    void* ptrs[16];

    for (size_t i = 0; i < count; ++i) {
        ptrs[i] = okm_arena_alloc(&arena, block);
        TEST_ASSERT_NOT_NULL(ptrs[i]);
        memset(ptrs[i], (int)i, block);
    }

    for (size_t i = 0; i < count; ++i) {
        uint8_t expected = (uint8_t)i;
        uint8_t* bytes = (uint8_t*)ptrs[i];
        for (size_t j = 0; j < block; ++j) {
            TEST_ASSERT_EQUAL_UINT8(expected, bytes[j]);
        }
    }

    okm_arena_destroy(&arena);
}

void test_ArenaAllocChunkOverflow(void) {
    OkmArena arena;
    okm_arena_init(&arena);

    const size_t chunk_size = 256u * 1024u;
    const size_t alloc_size = chunk_size / 2;

    void* first = okm_arena_alloc(&arena, alloc_size);
    TEST_ASSERT_NOT_NULL(first);

    void* second = okm_arena_alloc(&arena, alloc_size);
    TEST_ASSERT_NOT_NULL(second);

    void* third = okm_arena_alloc(&arena, alloc_size);
    TEST_ASSERT_NOT_NULL(third);

    TEST_ASSERT_NOT_NULL(arena.head->next);

    okm_arena_destroy(&arena);
}

void test_ArenaReset(void) {
    OkmArena arena;
    okm_arena_init(&arena);

    void* before = okm_arena_alloc(&arena, 128);
    TEST_ASSERT_NOT_NULL(before);

    okm_arena_reset(&arena);

    TEST_ASSERT_EQUAL_PTR(arena.head, arena.active);
    TEST_ASSERT_EQUAL_size_t(0u, arena.head->offset);

    void* after = okm_arena_alloc(&arena, 128);
    TEST_ASSERT_EQUAL_PTR(before, after);

    okm_arena_destroy(&arena);
}

void test_ArenaDestroy(void) {
    OkmArena arena;
    okm_arena_init(&arena);

    okm_arena_alloc(&arena, 256);
    okm_arena_destroy(&arena);

    TEST_ASSERT_NULL(arena.head);
    TEST_ASSERT_NULL(arena.active);
}

int main(void) {
    UNITY_BEGIN();

    RUN_TEST(test_ArenaInit);
    RUN_TEST(test_ArenaAllocBasic);
    RUN_TEST(test_ArenaAllocAlignment);
    RUN_TEST(test_ArenaAllocNonOverlap);
    RUN_TEST(test_ArenaAllocChunkOverflow);
    RUN_TEST(test_ArenaReset);
    RUN_TEST(test_ArenaDestroy);

    return UNITY_END();
}
