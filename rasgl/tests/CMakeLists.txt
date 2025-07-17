add_executable(tests
    main.c
    pipeline_tests.c)

include(${CMAKE_SOURCE_DIR}/cmake/FindLarse.cmake)
target_link_libraries(tests PRIVATE core)

target_include_directories(tests PUBLIC include)

add_compile_options(-fsanitize=address)
add_link_options(-fsanitize=address)

if(MSVC)
  target_compile_options(tests PRIVATE /W4 /WX)
else()
  target_compile_options(tests PRIVATE -Wall -Wextra -Wpedantic -Werror)
endif()
