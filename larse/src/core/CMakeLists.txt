add_library(larse_core
    src/parse.c)

target_include_directories(larse_core PUBLIC include)

target_link_libraries(larse_core PUBLIC)
