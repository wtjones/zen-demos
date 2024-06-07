add_executable(tests
    main.c
    test_support.c)


target_link_libraries(tests PRIVATE larse_core)

target_include_directories(tests PUBLIC include)

enable_testing()

add_test(
    NAME TEST_PARSE
    COMMAND $<TARGET_FILE:tests> TEST_PARSE)

add_test(
    NAME TEST_PARSE_SCRIPT
    COMMAND $<TARGET_FILE:tests> TEST_PARSE_SCRIPT)

add_test(
    NAME TEST_PARSE_SINGLE
    COMMAND $<TARGET_FILE:tests> TEST_PARSE_SINGLE)

add_test(
    NAME TEST_PARSE_SYMBOL
    COMMAND $<TARGET_FILE:tests> TEST_PARSE_SYMBOL)

add_test(
    NAME TEST_REPR
    COMMAND $<TARGET_FILE:tests> TEST_REPR)
