add_executable(rascli
    src/main.c
)

set_target_properties(rascli PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)

include(${CMAKE_SOURCE_DIR}/cmake/FindLarse.cmake)

target_include_directories(rascli PUBLIC include)
target_link_libraries(rascli PRIVATE m core hosted pack)
