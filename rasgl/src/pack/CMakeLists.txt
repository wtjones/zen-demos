
add_library(pack
    common.c
    element_encode.c
    element_decode.c
    model_encode.c
    model_decode.c
    scene_encode.c
    scene_decode.c)

target_include_directories(pack PUBLIC include)
target_link_libraries(pack PRIVATE core mpack)
