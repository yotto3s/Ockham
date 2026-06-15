include(FetchContent)

FetchContent_Declare(
    unity
    URL https://github.com/ThrowTheSwitch/Unity/archive/refs/tags/v2.6.0.tar.gz
    DOWNLOAD_EXTRACT_TIMESTAMP TRUE
)

FetchContent_MakeAvailable(unity)

add_executable(arena_test arena_test.c)

target_include_directories(arena_test
    PRIVATE
        ${PROJECT_SOURCE_DIR}/lib
)

target_link_libraries(arena_test
    PRIVATE
        ockham
        unity
)

add_test(NAME arena_test COMMAND arena_test)
