add_library(core  maths.c maths_tables.c matrix.c repr.c)
target_include_directories(core PUBLIC include )
