add_library(ockham STATIC
    arena.c
    context.c
    instruction.c
    function.c
    block.c
    print.c
    parse.c
    x86_64/emit.c
)

target_include_directories(ockham
    PUBLIC
        ${PROJECT_SOURCE_DIR}/include
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}
)
