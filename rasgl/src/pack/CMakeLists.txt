
add_library(pack
    element_encode.c
    model_encode.c
    model_decode.c
    scene_encode.c
    scene_decode.c)

target_include_directories(pack PUBLIC include)
target_link_libraries(pack PRIVATE core mpack)
