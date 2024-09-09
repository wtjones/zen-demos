add_executable(tests
    main.c
    test_support.c)


target_link_libraries(tests PRIVATE rexile_core external)

target_include_directories(tests PUBLIC include)
target_compile_options(tests PRIVATE -Wall -Wextra -Wpedantic -Werror)

enable_testing()

add_test(
    NAME test_stack_can_push
    COMMAND $<TARGET_FILE:tests> test_stack_can_push)

add_test(
    NAME test_stack_can_pop
    COMMAND $<TARGET_FILE:tests> test_stack_can_pop)

add_test(
    NAME test_stack_can_populate
    COMMAND $<TARGET_FILE:tests> test_stack_can_populate)

add_test(
    NAME test_empty_deck_loses
    COMMAND $<TARGET_FILE:tests> test_empty_deck_loses)

add_test(
    NAME test_invalid_placement_returns_invald
    COMMAND $<TARGET_FILE:tests> test_invalid_placement_returns_invald)

add_test(
    NAME test_unable_to_place_face_card_loses
    COMMAND $<TARGET_FILE:tests> test_unable_to_place_face_card_loses)

add_test(
    NAME test_combine_allows_valid_cards
    COMMAND $<TARGET_FILE:tests> test_combine_allows_valid_cards)
