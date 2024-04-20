add_executable(tests
    main.c
    test_support.c)


target_link_libraries(tests PRIVATE larse_core)

target_include_directories(tests PUBLIC include)

enable_testing()

add_test(
    NAME TEST_STRING
    COMMAND $<TARGET_FILE:tests> TEST_STRING)

add_test(
    NAME TEST_SYMBOL
    COMMAND $<TARGET_FILE:tests> TEST_SYMBOL)

