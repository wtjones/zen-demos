add_executable(tests
    main.c)

target_link_libraries(tests PRIVATE core)

target_include_directories(tests PUBLIC include)
