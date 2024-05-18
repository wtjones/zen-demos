add_executable(tests
    main.c
    test_support.c)


target_link_libraries(tests larse_core external_unity  )

#target_include_directories(tests PUBLIC include)

enable_testing()

add_test(NAME foo_tests COMMAND $<TARGET_FILE:tests> )

#add_test(NAME foo_tests COMMAND tests )


# add_test(
#     NAME TEST_PARSE_SINGLE
#     COMMAND $<TARGET_FILE:tests> TEST_PARSE_SINGLE)

# add_test(
#     NAME TEST_PARSE
#     COMMAND $<TARGET_FILE:tests> TEST_PARSE)

# add_test(
#     NAME TEST_PARSE_SYMBOL
#     COMMAND $<TARGET_FILE:tests> TEST_PARSE_SYMBOL)

# add_test(
#     NAME TEST_REPR
#     COMMAND $<TARGET_FILE:tests> TEST_REPR)
