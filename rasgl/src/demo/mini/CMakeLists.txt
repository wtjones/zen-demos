IF (NOT RAS_PLATFORM_TYPE STREQUAL "embedded")
  add_executable(demo
      demo_main.c)

  include(${CMAKE_SOURCE_DIR}/cmake/FindLarse.cmake)

  target_link_libraries(demo PRIVATE core ${RAS_PLATFORM})

  target_include_directories(demo PUBLIC include)

  if(MSVC)
    target_compile_options(demo PRIVATE /W4 /WX)
  else()
    target_compile_options(demo PRIVATE -Wall -Wextra -Werror)
  endif()

else()
  # Treat as a library
  add_library(demo
      demo_main.c)

  target_include_directories(demo PUBLIC core include)
  target_link_libraries(demo PRIVATE core)

ENDIF()
