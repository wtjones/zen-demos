add_executable(larse src/main.c)

target_link_libraries(larse PRIVATE larse_core)

target_include_directories(larse PUBLIC include)
