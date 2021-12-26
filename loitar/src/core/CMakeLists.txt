add_library(loitar_core
    src/atom_node.cpp
    src/environment.cpp
    src/evaluator.cpp
    src/integer_node.cpp
    src/list_node.cpp
    src/node.cpp
    src/parser.cpp
    src/repl.cpp
    src/string_node.cpp
    src/syslib.cpp)

target_compile_features(loitar_core PUBLIC cxx_std_17)

target_include_directories(loitar_core PUBLIC include)

target_link_libraries(loitar_core PUBLIC spdlog::spdlog)
