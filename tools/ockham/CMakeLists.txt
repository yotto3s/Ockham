add_executable(ockham_driver main.c)

set_target_properties(ockham_driver PROPERTIES OUTPUT_NAME ockham)

target_include_directories(ockham_driver
    PRIVATE
        ${PROJECT_SOURCE_DIR}/lib
)

target_link_libraries(ockham_driver
    PRIVATE
        ockham
)

if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(ockham_driver PRIVATE -Wall -Wextra -Wpedantic)
endif()
