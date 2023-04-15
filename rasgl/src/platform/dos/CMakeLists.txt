add_library(ras_dos main.c)

INCLUDE_DIRECTORIES(  "$ENV{ALLEGRO}/include" )
LINK_DIRECTORIES(  "$ENV{ALLEGRO}/lib/djgpp" )

file(GLOB LIBRARIES "$ENV{ALLEGRO}/lib/djgpp/*.a")
message("ALLEGRO = $ENV{ALLEGRO}")
message("LIBRARIES = ${LIBRARIES}")

target_include_directories(ras_dos PUBLIC include)
target_link_libraries(ras_dos PRIVATE raslib ${LIBRARIES})
