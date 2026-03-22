#include "rasgl/core/debug.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/gridmap.h"
#include "rasgl/hosted/sc_load.h"

RasResult hosted_script_map_gridmap(LarNode* gridmap_exp, RasSceneGridMap* gridmap)
{

    memset(gridmap, 0, sizeof(RasSceneGridMap));
    gridmap->height = 1; // Currently only 2d gridmaps supported.

    LarNode* name_node = lar_get_property_by_type(
        gridmap_exp, SCRIPT_SYMBOL_GRIDMAP_NAME, LAR_NODE_ATOM_STRING);

    gridmap->name[0] = '\0';
    if (name_node == NULL) {
        return RAS_RESULT_ERROR;
    }

    strncpy(gridmap->name, name_node->atom.val_symbol, MAX_GRIDMAP_NAME - 1);

    LarNode* sz_node = lar_get_property_by_type(
        gridmap_exp, SCRIPT_SYMBOL_GRIDMAP_WIDTH, LAR_NODE_ATOM_INTEGER);

    RAS_CHECK_AND_LOG(sz_node == NULL,
        "Failed to find property %s", SCRIPT_SYMBOL_GRIDMAP_WIDTH);

    gridmap->width = sz_node->atom.val_integer;

    sz_node = lar_get_property_by_type(
        gridmap_exp, SCRIPT_SYMBOL_GRIDMAP_CELL_SIZE, LAR_NODE_ATOM_FIXED);

    RAS_CHECK_AND_LOG(sz_node == NULL,
        "Failed to find property %s", SCRIPT_SYMBOL_GRIDMAP_CELL_SIZE);

    gridmap->cell_size = sz_node->atom.val_fixed;

    sz_node = lar_get_property_by_type(
        gridmap_exp, SCRIPT_SYMBOL_GRIDMAP_DEPTH, LAR_NODE_ATOM_INTEGER);

    RAS_CHECK_AND_LOG(sz_node == NULL,
        "Failed to find property %s", SCRIPT_SYMBOL_GRIDMAP_DEPTH);

    gridmap->depth = sz_node->atom.val_integer;

    LarNode* cells_node = lar_get_property_by_type(
        gridmap_exp, SCRIPT_SYMBOL_GRIDMAP_CELLS, LAR_NODE_LIST);

    RAS_CHECK_AND_LOG(cells_node == NULL,
        "Failed to find property %s", SCRIPT_SYMBOL_GRIDMAP_CELLS);

    RAS_CHECK_AND_LOG(cells_node->list.length != gridmap->depth,
        "Gridmap cells row count does not match depth");

    for (size_t r = 0; r < gridmap->depth; r++) {
        LarNode* row_node = &cells_node->list.nodes[r];
        RAS_CHECK_AND_LOG(row_node->list.length != gridmap->width,
            "Gridmap cells column count does not match width");
        for (size_t c = 0; c < gridmap->width; c++) {
            size_t dst_index = (r * gridmap->width) + c;
            LarNode* cell_node = &row_node->list.nodes[c];
            gridmap->cells[dst_index].material = cell_node->atom.val_integer;
        }
    }

    core_set_gridmap_cell_flags(gridmap);
    if (core_gridmap_to_element_verts_alloc(gridmap, &gridmap->element) != RAS_RESULT_OK) {
        ras_log_error("Failed to allocate memory for gridmap pipeline element.");
        return RAS_RESULT_ERROR;
    }

    if (core_gridmap_to_element_verts(gridmap, &gridmap->element) != RAS_RESULT_OK) {
        ras_log_error("Failed to convert gridmap to pipeline element.");
        return RAS_RESULT_ERROR;
    }

    return RAS_RESULT_OK;
}

RasResult hosted_script_map_gridmaps(
    LarNode* scene_exp,
    RasSceneGridMap** out_gridmaps,
    size_t* out_num_gridmaps)
{
    LarNode* list = lar_get_list_by_symbol(
        scene_exp, SCRIPT_SYMBOL_GRIDMAPS);

    if (list == NULL) {
        *out_num_gridmaps = 0;
        ras_log_info("Gridmaps list not found in script");
        return RAS_RESULT_OK;
    }

    size_t num_gridmaps = list->list.length - 1; // exclude the symbol

    RasSceneGridMap* gridmaps = (RasSceneGridMap*)malloc(
        sizeof(RasSceneGridMap) * num_gridmaps);

    RAS_CHECK_AND_LOG(gridmaps == NULL,
        "Failed to allocate memory for scene gridmaps");

    for (size_t i = 0; i < num_gridmaps; i++) {
        // skip the symbol
        LarNode* gridmap_exp = lar_get_list_node_by_index(list, i + 1);

        RasSceneGridMap* gridmap = &gridmaps[i];

        RasResult result = hosted_script_map_gridmap(gridmap_exp, gridmap);

        if (result != RAS_RESULT_OK) {
            ras_log_error("Failed to map gridmap");
            free(gridmaps);
            return RAS_RESULT_ERROR;
        }
    }

    *out_gridmaps = gridmaps;
    *out_num_gridmaps = num_gridmaps;

    return RAS_RESULT_OK;
}
