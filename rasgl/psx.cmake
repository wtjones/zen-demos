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
	src/platform/psx/vendor/printf.c
)
target_include_directories(
	common PUBLIC
	src/platform/psx
	src/platform/psx/libc
)

include_directories(
	${CMAKE_SOURCE_DIR}/src/platform/psx
	${CMAKE_SOURCE_DIR}/src/platform/psx/libc)

add_subdirectory(src)

target_include_directories(core PUBLIC
	src/platform/psx
	src/platform/psx/libc
)

addPS1Executable(ras_psx
	src/platform/psx/main.c
	src/platform/psx/serial.c
	src/platform/psx/log_impl.c
)

target_link_libraries(ras_psx PRIVATE common core)

addBinaryFileWithSize(ras_psx textData textDataSize src/platform/psx/data.txt)
