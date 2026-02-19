project(
	RasGL
	LANGUAGES    C CXX ASM
	VERSION      1.0.0
	DESCRIPTION  "RasGL PSX target"
)

include(cmake-psx/setup.cmake)
include(cmake-psx/tools.cmake)

set (RAS_PLATFORM_TYPE "embedded")

add_library(
	common OBJECT
	src/platform/psx/libc/clz.s
	src/platform/psx/libc/crt0.c
	src/platform/psx/libc/cxxsupport.cpp
	src/platform/psx/libc/malloc.c
	src/platform/psx/libc/misc.c
	src/platform/psx/libc/setjmp.s
	src/platform/psx/libc/string.c
	src/platform/psx/libc/string.s
	src/platform/psx/ps1/cache.s
	src/platform/psx/ps1/gpu.c
	src/platform/psx/vendor/printf.c
)

# Enable floating point in snprintf() log usage.
target_compile_definitions(common PRIVATE PRINTF_SUPPORT_FLOAT=1)

target_include_directories(
	common PUBLIC
	src/platform/psx
	src/platform/psx/libc
)

include_directories(
	${CMAKE_SOURCE_DIR}/src/platform/psx
	${CMAKE_SOURCE_DIR}/src/platform/psx/libc)

add_compile_definitions(RAS_LOG_BUFFER_MODE=${RAS_LOG_BUFFER_MODE})

add_subdirectory(src)

target_include_directories(core PUBLIC
	src/platform/psx
	src/platform/psx/libc
)

addPS1Executable(ras_psx
	src/platform/psx/main.c
	src/platform/psx/serial.c
	src/platform/psx/log_impl.c
	src/platform/psx/render.c
	src/platform/psx/sc_load.c
)

target_link_libraries(ras_psx PRIVATE common core pack demo)

addBinaryFileWithSize(ras_psx textData textDataSize src/platform/psx/data.txt)
addBinaryFileWithSize(ras_psx scene_data scene_data_size bld_psx/scene.mp)

MESSAGE(STATUS "RAS_MAX_FRAMES: ${RAS_MAX_FRAMES}")

if(DEFINED RAS_MAX_FRAMES)
  target_compile_definitions(ras_psx PRIVATE RAS_MAX_FRAMES=${RAS_MAX_FRAMES})
endif()
