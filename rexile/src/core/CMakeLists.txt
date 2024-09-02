add_library(rexile_core STATIC
    src/board.c
    src/deck.c
    src/game.c
    src/repr.c)

target_include_directories(rexile_core PUBLIC include)

target_link_libraries(rexile_core PRIVATE external m PUBLIC)
