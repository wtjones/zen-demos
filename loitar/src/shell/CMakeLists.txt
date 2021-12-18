add_executable(loitar src/main.cpp)

target_include_directories(loitar PUBLIC include)

target_compile_features(loitar PUBLIC cxx_std_17)

target_link_libraries(loitar PRIVATE loitar_core spdlog::spdlog)
