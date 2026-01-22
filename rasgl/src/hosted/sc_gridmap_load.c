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
