project(
	RasGL
	LANGUAGES    C CXX ASM
	VERSION      1.0.0
	DESCRIPTION  "RasGL PSX target"
)

include(cmake-psx/setup.cmake)
include(cmake-psx/tools.cmake)

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
link_libraries(common)

addPS1Executable(RasGL
	src/platform/psx/main.c
	src/platform/psx/serial.c
)

addBinaryFileWithSize(RasGL textData textDataSize src/platform/psx/data.txt)
