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

target_include_directories(larse_core PUBLIC include)

target_link_libraries(larse_core PRIVATE external m PUBLIC)
