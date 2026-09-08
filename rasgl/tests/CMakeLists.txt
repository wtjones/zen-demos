add_executable(tests
    main.c
    backface_tests.c
    fixed_maths_tests.c
    gridmap_tests.c
    line_buffer_tests.c
    maths_tests.c
    matrix_tests.c
    pack_tests.c
    pipeline_tests.c
    plane_tests.c
    tombmap_tests.c
    test_support.c
    scene_tests.c)

include(${CMAKE_SOURCE_DIR}/cmake/FindLarse.cmake)

target_include_directories(tests PUBLIC include)
target_link_libraries(tests PRIVATE m core hosted pack)

add_compile_options(-fsanitize=address)
add_link_options(-fsanitize=address)

if(MSVC)
  target_compile_options(tests PRIVATE /W4 /WX)
else()
  target_compile_options(tests PRIVATE -Wall -Wextra -Wpedantic -Werror)
endif()

enable_testing()

# Generate add_test() calls from TestList.inc
file(STRINGS TestList.inc TEST_LINES)

foreach(line ${TEST_LINES})
    string(REGEX REPLACE "X\\((.*)\\)" "\\1" test "${line}")
    add_test(NAME ${test}
             COMMAND $<TARGET_FILE:tests> ${test}
              WORKING_DIRECTORY ${CMAKE_SOURCE_DIR})
endforeach()
