add_executable(tests
    main.c
    test_support.c)


#target_link_libraries(tests PRIVATE rexile)

target_include_directories(tests PUBLIC include)


enable_testing()

add_test(
    NAME test_empty_deck_loses
    COMMAND $<TARGET_FILE:tests> test_empty_deck_loses)

add_test(
    NAME test_invalid_placement_returns_invald
    COMMAND $<TARGET_FILE:tests> test_invalid_placement_returns_invald)
