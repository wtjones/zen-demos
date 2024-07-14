add_executable(larse src/main.c)

target_link_libraries(larse PRIVATE external larse_core)

target_include_directories(larse PRIVATE external PUBLIC include)
