add_library(core  maths.c maths_tables.c matrix.c repr.c graphics.c)
target_include_directories(core PUBLIC include )
