cmake_minimum_required(VERSION 3.20)

project(Ockham
    VERSION 0.1.0
    DESCRIPTION "A minimal compiler backend"
    LANGUAGES C
)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

enable_testing()

add_subdirectory(lib)
add_subdirectory(tools)
add_subdirectory(tests)
