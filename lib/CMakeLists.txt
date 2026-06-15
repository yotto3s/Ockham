add_library(ockham STATIC
    arena.c
    instruction.c
    function.c
    block.c
)

target_include_directories(ockham
    PUBLIC
        ${PROJECT_SOURCE_DIR}/include
    PRIVATE
        ${CMAKE_CURRENT_SOURCE_DIR}
)
