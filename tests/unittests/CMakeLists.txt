include(FetchContent)

FetchContent_Declare(
    unity
    URL https://github.com/ThrowTheSwitch/Unity/archive/refs/tags/v2.6.0.tar.gz
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)

FetchContent_MakeAvailable(unity)

set(UNITTEST_INCLUDE_DIRS
    ${PROJECT_SOURCE_DIR}/lib
    ${PROJECT_SOURCE_DIR}/include
)

# --- arena_test ---
add_executable(arena_test arena_test.c)
target_include_directories(arena_test PRIVATE ${UNITTEST_INCLUDE_DIRS})
target_link_libraries(arena_test PRIVATE ockham unity)
add_test(NAME arena_test COMMAND arena_test)

# --- function_test ---
add_executable(function_test function_test.c)
target_include_directories(function_test PRIVATE ${UNITTEST_INCLUDE_DIRS})
target_link_libraries(function_test PRIVATE ockham unity)
add_test(NAME function_test COMMAND function_test)

# --- block_test ---
add_executable(block_test block_test.c)
target_include_directories(block_test PRIVATE ${UNITTEST_INCLUDE_DIRS})
target_link_libraries(block_test PRIVATE ockham unity)
add_test(NAME block_test COMMAND block_test)

# --- instruction_test ---
add_executable(instruction_test instruction_test.c)
target_include_directories(instruction_test PRIVATE ${UNITTEST_INCLUDE_DIRS})
target_link_libraries(instruction_test PRIVATE ockham unity)
add_test(NAME instruction_test COMMAND instruction_test)

# --- context_test ---
add_executable(context_test context_test.c)
target_include_directories(context_test PRIVATE ${UNITTEST_INCLUDE_DIRS})
target_link_libraries(context_test PRIVATE ockham unity)
add_test(NAME context_test COMMAND context_test)

# --- emit_test ---
add_executable(emit_test emit_test.c)
target_include_directories(emit_test PRIVATE ${UNITTEST_INCLUDE_DIRS})
target_link_libraries(emit_test PRIVATE ockham unity)
add_test(NAME emit_test COMMAND emit_test)

# --- print_test ---
add_executable(print_test print_test.c)
target_include_directories(print_test PRIVATE ${UNITTEST_INCLUDE_DIRS})
target_link_libraries(print_test PRIVATE ockham unity)
add_test(NAME print_test COMMAND print_test)

# --- parse_test ---
add_executable(parse_test parse_test.c)
target_include_directories(parse_test PRIVATE ${UNITTEST_INCLUDE_DIRS})
target_link_libraries(parse_test PRIVATE ockham unity)
add_test(NAME parse_test COMMAND parse_test)


