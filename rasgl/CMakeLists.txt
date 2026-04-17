cmake_minimum_required(VERSION 3.7)


if (NOT RAS_PLATFORM STREQUAL "ras_psx")


  project(
    RasGL
    VERSION 1.0
    DESCRIPTION ""
    LANGUAGES C)


  set(RAS_PLATFORM_TYPE "hosted" CACHE STRING "Platform type: hosted or embedded")
  set(RAS_LOG_BUFFER_MODE "1" CACHE STRING "Log buffer mode: 0=disabled, 1=enabled")
  set(RAS_NDC_LUT "1" CACHE STRING "Use NDC LUT: 0=disabled, 1=enabled")
  add_compile_definitions(
    DEBUG=${DEBUG}
    RAS_LOG_BUFFER_MODE=${RAS_LOG_BUFFER_MODE}
    RAS_NDC_LUT=${RAS_NDC_LUT}
    RAS_PLATFORM_HOSTED)

  set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

  option(ENABLE_ASAN "Enable AddressSanitizer" OFF)

  if(ENABLE_ASAN)
    add_compile_options(-fsanitize=address -fno-omit-frame-pointer -g -fno-sanitize=leak)
    add_link_options(-fsanitize=address -fno-sanitize=leak)
  endif()

  add_subdirectory(src)
  add_subdirectory(tests)

else()

  include (psx.cmake)

endif()
