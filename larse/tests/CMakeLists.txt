add_executable(tests
    main.c
    test_support.c)


target_link_libraries(tests PRIVATE larse_core)

target_include_directories(tests PUBLIC include)

target_compile_definitions(tests PRIVATE TEST_DATA_DIR="${CMAKE_CURRENT_SOURCE_DIR}/data")

enable_testing()

add_test(
    NAME TEST_PARSE
    COMMAND $<TARGET_FILE:tests> TEST_PARSE)

add_test(
    NAME TEST_PARSE_FILE
    COMMAND $<TARGET_FILE:tests> TEST_PARSE_FILE)

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
    NAME TEST_REPR_EXPRESSION
    COMMAND $<TARGET_FILE:tests> TEST_REPR_EXPRESSION)

add_test(
    NAME TEST_REPR_SCRIPT
    COMMAND $<TARGET_FILE:tests> TEST_REPR_SCRIPT)
