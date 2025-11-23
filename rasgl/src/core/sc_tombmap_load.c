#include "rasgl/core/tombmap.h"

RasResult core_script_map_tombmap_sector(LarNode* sector_list, RasTombMapSector* sector)
{
    memset(sector, 0, sizeof(RasTombMapSector));

    LarNode* material_node = lar_get_list_node_by_index(sector_list, 0);

    RAS_CHECK_AND_LOG(material_node == NULL,
        "Failed to find sector material.");

    sector->material = material_node->atom.val_integer;

    LarNode* ceiling_node = lar_get_list_node_by_index(sector_list, 1);

    RAS_CHECK_AND_LOG(ceiling_node == NULL,
        "Failed to find sector ceiling.");

    sector->ceiling = (int8_t)ceiling_node->atom.val_integer;

    LarNode* floor_node = lar_get_list_node_by_index(sector_list, 2);

    RAS_CHECK_AND_LOG(floor_node == NULL,
        "Failed to find sector floor.");

    sector->floor = (int8_t)floor_node->atom.val_integer;

    return RAS_RESULT_OK;
}

RasResult core_script_map_tombmap_sectors(
    LarNode* rooms_node, RasTombMapRoom* room)
{
    LarNode* sectors_node = lar_get_list_by_symbol(
        rooms_node, SCRIPT_SYMBOL_TOMBMAP_SECTORS);

    RAS_CHECK_AND_LOG(sectors_node == NULL,
        "Failed to find property %s", SCRIPT_SYMBOL_TOMBMAP_SECTORS);

    LarNode* sectors_list = lar_get_list_node_by_index(sectors_node, 1);

    RAS_CHECK_AND_LOG(sectors_list == NULL,
        "Failed to find sectors list.");

    room->num_sectors_x = 0;
    room->num_sectors_z = sectors_list->list.length;

    RAS_CHECK_AND_LOG(sectors_list->list.length == 0,
        "Tombmap room has zero rows of sectors.");

    LarNode* first_sector_row = lar_get_list_node_by_index(sectors_list, 0);

    RAS_CHECK_AND_LOG(first_sector_row->list.length == 0,
        "Unable to determine width of room sectors.");

    room->num_sectors_x = first_sector_row->list.length;
    room->sectors = malloc(
        sizeof(RasTombMapSector) * room->num_sectors_z * room->num_sectors_x);

    RAS_CHECK_AND_LOG(room->sectors == NULL,
        "Failed to allocate memory for tombmap room sectors");

    for (size_t row = 0; row < room->num_sectors_z; row++) {
        LarNode* sector_row = &sectors_list->list.nodes[row];

        if (sector_row->list.length > room->num_sectors_x) {
            ras_log_error("Tombmap room sector row has %d items, but expected %d.",
                sector_row->list.length,
                room->num_sectors_x);
            free(room->sectors);
            return RAS_RESULT_ERROR;
        }
        for (size_t col = 0; col < room->num_sectors_x; col++) {
            LarNode* sector_node = &sector_row->list.nodes[col];

            RasTombMapSector* sector = &room->sectors[(row * room->num_sectors_x) + col];

            RasResult result = core_script_map_tombmap_sector(sector_node, sector);

            if (result != RAS_RESULT_OK) {
                ras_log_error("Failed to map tombmap sector.");
                free(room->sectors);
                return RAS_RESULT_ERROR;
            }
        }
    }
    return RAS_RESULT_OK;
}

RasResult core_script_map_tombmap_room(LarNode* exp, RasTombMapRoom* room)
{

    memset(room, 0, sizeof(RasTombMapRoom));

    LarNode* x_node = lar_get_property_by_type(
        exp, SCRIPT_SYMBOL_TOMBMAP_ROOM_X, LAR_NODE_ATOM_INTEGER);

    RAS_CHECK_AND_LOG(x_node == NULL,
        "Failed to find property %s", SCRIPT_SYMBOL_TOMBMAP_ROOM_X);

    room->x = x_node->atom.val_integer;

    LarNode* y_top_node = lar_get_property_by_type(
        exp, SCRIPT_SYMBOL_TOMBMAP_ROOM_Y_TOP, LAR_NODE_ATOM_INTEGER);

    RAS_CHECK_AND_LOG(y_top_node == NULL,
        "Failed to find property %s", SCRIPT_SYMBOL_TOMBMAP_ROOM_Y_TOP);

    room->y_top = y_top_node->atom.val_integer;

    LarNode* y_bottom_node = lar_get_property_by_type(
        exp, SCRIPT_SYMBOL_TOMBMAP_ROOM_Y_BOTTOM, LAR_NODE_ATOM_INTEGER);

    RAS_CHECK_AND_LOG(y_bottom_node == NULL,
        "Failed to find property %s", SCRIPT_SYMBOL_TOMBMAP_ROOM_Y_BOTTOM);

    room->y_bottom = y_bottom_node->atom.val_integer;

    RAS_CHECK_AND_LOG(
        core_script_map_tombmap_sectors(exp, room),
        "Failed to map sectors of room.");

    return RAS_RESULT_OK;
}

RasResult core_script_map_tombmap_rooms(
    LarNode* rooms_node, RasTombMapRoom** out_rooms, size_t* out_num_rooms)
{
    size_t num_rooms = rooms_node->list.length - 1; // exclude symbol

    RasTombMapRoom* rooms = malloc(sizeof(RasTombMapRoom) * num_rooms);

    RAS_CHECK_AND_LOG(rooms == NULL,
        "Failed to allocate memory for tombmap rooms");

    for (size_t i = 0; i < num_rooms; i++) {
        LarNode* room_exp = lar_get_list_node_by_index(rooms_node, i + 1); // Skip the symbol

        RasTombMapRoom* room = &rooms[i];

        RasResult result = core_script_map_tombmap_room(room_exp, room);

        if (result != RAS_RESULT_OK) {
            ras_log_error("Failed to map tombmap room");
            free(rooms);
            return RAS_RESULT_ERROR;
        }
    }

    *out_rooms = rooms;
    *out_num_rooms = num_rooms;

    return RAS_RESULT_OK;
}

RasResult core_script_map_tombmap(LarNode* exp, RasSceneTombMap* map)
{
    memset(map, 0, sizeof(RasSceneTombMap));

    LarNode* name_node = lar_get_property_by_type(
        exp, SCRIPT_SYMBOL_TOMBMAP_NAME, LAR_NODE_ATOM_STRING);

    map->name[0] = '\0';
    if (name_node == NULL) {
        return RAS_RESULT_ERROR;
    }

    strncpy(map->name, name_node->atom.val_symbol, MAX_TOMBMAP_NAME - 1);

    LarNode* rooms_node = lar_get_list_by_symbol(
        exp, SCRIPT_SYMBOL_TOMBMAP_ROOMS);

    RAS_CHECK_RESULT_AND_LOG(
        core_script_map_tombmap_rooms(rooms_node, &map->rooms, &map->num_rooms),
        "Failed to map tombmap rooms");

    return RAS_RESULT_OK;
}

RasResult core_script_map_tombmaps(
    LarNode* scene_exp,
    RasSceneTombMap** out_tombmaps,
    size_t* out_num_tombmaps)
{

    LarNode* tombmaps_exp = lar_get_list_by_symbol(
        scene_exp, SCRIPT_SYMBOL_TOMBMAPS);

    if (tombmaps_exp == NULL) {
        *out_num_tombmaps = 0;
        ras_log_info("Tombmaps in script: 0");
        return RAS_RESULT_OK;
    }

    size_t num_tombmaps = tombmaps_exp->list.length - 1; // exclude symbol
    RasSceneTombMap* maps = malloc(sizeof(RasSceneTombMap) * num_tombmaps);

    RAS_CHECK_AND_LOG(maps == NULL,
        "Failed to allocate memory for tombmaps");

    for (size_t i = 0; i < num_tombmaps; i++) {
        LarNode* map_exp = lar_get_list_node_by_index(tombmaps_exp, i + 1); // Skip symbol

        RasSceneTombMap* map = &maps[i];

        RasResult result = core_script_map_tombmap(map_exp, map);

        if (result != RAS_RESULT_OK) {
            ras_log_error("Failed to map tombmap.");
            free(maps);
            return RAS_RESULT_ERROR;
        }
    }

    *out_tombmaps = maps;
    *out_num_tombmaps = num_tombmaps;

    if (core_tombmap_to_element_verts(&maps[0]) != RAS_RESULT_OK) {
        ras_log_error("Failed to convert gridmap to pipeline element.");
        return RAS_RESULT_ERROR;
    }

    return RAS_RESULT_OK;
}

void core_free_scene_tombmaps(RasSceneTombMap* tombmaps, size_t num_tombmaps)
{
    if (num_tombmaps == 0) {
        return;
    }

    for (size_t i = 0; i < num_tombmaps; i++) {
        RasSceneTombMap* map = &tombmaps[i];
        for (size_t j = 0; j < map->num_rooms; j++) {
            RasTombMapRoom* room = &map->rooms[j];
            if (room->num_sectors_x * room->num_sectors_z == 0) {
                continue;
            }
            free(room->sectors);
        }
        free(map->rooms);
    }

    free(tombmaps);
}
