if (${RAS_DEMO} STREQUAL "world")
    add_subdirectory(world)
elseif (${RAS_DEMO} STREQUAL "poly")
    add_subdirectory(poly)
endif()
