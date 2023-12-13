add_executable(tests
    main.c)

target_link_libraries(tests PRIVATE core)

target_include_directories(tests PUBLIC include)

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fsanitize=address")

if(MSVC)
  target_compile_options(tests PRIVATE /W4 /WX)
else()
  target_compile_options(tests PRIVATE -Wall -Wextra -Wpedantic -Werror)
endif()
