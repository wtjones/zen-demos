add_library(ras_dos main.c)

target_include_directories(ras_dos PUBLIC include)
target_link_libraries(ras_dos PRIVATE raslib)
