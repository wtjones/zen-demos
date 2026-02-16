
add_library(pack
    common.c
    element_encode.c
    element_decode.c
    face_encode.c
    face_decode.c
    camera_encode.c
    camera_decode.c
    object_encode.c
    object_decode.c
    model_encode.c
    model_decode.c
    scene_encode.c
    scene_decode.c)

target_include_directories(pack PUBLIC include)
target_link_libraries(pack PRIVATE core mpack)
