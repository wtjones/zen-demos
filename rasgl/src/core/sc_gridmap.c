#include "rasgl/core/debug.h"
#include "rasgl/core/gridmap.h"

RasResult core_script_map_gridmap(LarNode* gridmap_exp, RasSceneGridMap* gridmap)
{

    memset(gridmap, 0, sizeof(RasSceneGridMap));

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
        gridmap_exp, SCRIPT_SYMBOL_GRIDMAP_HEIGHT, LAR_NODE_ATOM_INTEGER);

    RAS_CHECK_AND_LOG(sz_node == NULL,
        "Failed to find property %s", SCRIPT_SYMBOL_GRIDMAP_HEIGHT);

    gridmap->height = sz_node->atom.val_integer;

    LarNode* cells_node = lar_get_property_by_type(
        gridmap_exp, SCRIPT_SYMBOL_GRIDMAP_CELLS, LAR_NODE_LIST);

    RAS_CHECK_AND_LOG(cells_node == NULL,
        "Failed to find property %s", SCRIPT_SYMBOL_GRIDMAP_CELLS);

    for (size_t r = 0; r < gridmap->height; r++) {
        LarNode* row_node = &cells_node->list.nodes[r];

        for (size_t c = 0; c < gridmap->width; c++) {
            size_t dst_index = (r * gridmap->width) + c;
            LarNode* cell_node = &row_node->list.nodes[c];
            gridmap->cells[dst_index] = cell_node->atom.val_integer;
        }
    }

    return RAS_RESULT_OK;
}
