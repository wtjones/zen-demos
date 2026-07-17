
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/include/larse/core/version.h.in
    ${CMAKE_CURRENT_BINARY_DIR}/generated/larse/core/version.h
    @ONLY
)

add_library(larse_core STATIC
    src/clone.c
    src/expression.c
    src/io.c
    src/logging.c
    src/merge.c
    src/parse.c
    src/parse_token.c
    src/repr.c
)

target_include_directories(larse_core PUBLIC
    include
    ${CMAKE_CURRENT_BINARY_DIR}/generated)

target_link_libraries(larse_core PRIVATE external PUBLIC m)
