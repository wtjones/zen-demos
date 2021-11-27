add_library(loitar_core
    src/parser.cpp)

target_compile_features(loitar_core PUBLIC cxx_std_17)

target_include_directories(loitar_core PUBLIC include)

target_link_libraries(loitar_core PUBLIC)