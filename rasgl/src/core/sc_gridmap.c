#include "rasgl/core/debug.h"
#include "rasgl/core/gridmap.h"

RasResult core_gridmap_to_pipeline_element(
    RasSceneGridMap* gridmap,
    RasPipelineElement* element)
{
    if (gridmap->height != 1) {
        ras_log_error("Only gridmaps with height 1 are supported.");
        return RAS_RESULT_ERROR;
    }
    memset(element, 0, sizeof(RasPipelineElement));

    for (size_t y = 0; y <= gridmap->height; y++) {
        for (size_t z = 0; z <= gridmap->depth; z++) {
            for (size_t x = 0; x <= gridmap->width; x++) {
                RasVector3f* v = &element->verts[element->num_verts].position;
                element->num_verts++;

                v->x = mul_fixed_16_16_by_fixed_16_16(
                    INT_32_TO_FIXED_16_16(x),
                    gridmap->cell_size);
                v->y = mul_fixed_16_16_by_fixed_16_16(
                    INT_32_TO_FIXED_16_16(y),
                    gridmap->cell_size);
                v->z = mul_fixed_16_16_by_fixed_16_16(
                    INT_32_TO_FIXED_16_16(z),
                    gridmap->cell_size);
            }
        }
    }

    return RAS_RESULT_OK;
}

void core_set_gridmap_cell_flags(RasSceneGridMap* gridmap)
{
    for (size_t z = 0; z < gridmap->depth; z++) {
        for (size_t x = 0; x < gridmap->width; x++) {
            size_t cell_index = (z * gridmap->width) + x;
            RasGridMapCell* cell = &gridmap->cells[cell_index];
            RasGridMapCell* cell_z_minus = z == 0
                ? NULL
                : &gridmap->cells[((z - 1) * gridmap->width) + x];
            RasGridMapCell* cell_z_plus = z == gridmap->depth - 1
                ? NULL
                : &gridmap->cells[((z + 1) * gridmap->width) + x];
            RasGridMapCell* cell_x_minus = x == 0
                ? NULL
                : &gridmap->cells[(z * gridmap->width) + (x - 1)];
            RasGridMapCell* cell_x_plus = x == gridmap->width - 1
                ? NULL
                : &gridmap->cells[(z * gridmap->width) + (x + 1)];

            cell->spatial_flags = cell_z_minus != NULL && cell_z_minus->material > 0
                ? RAS_GRIDMAP_Z_MINUS_1
                : 0;
            cell->spatial_flags |= cell_z_plus != NULL && cell_z_plus->material > 0
                ? RAS_GRIDMAP_Z_PLUS_1
                : 0;
            cell->spatial_flags |= cell_x_minus != NULL && cell_x_minus->material > 0
                ? RAS_GRIDMAP_X_MINUS_1
                : 0;
            cell->spatial_flags |= cell_x_plus != NULL && cell_x_plus->material > 0
                ? RAS_GRIDMAP_X_PLUS_1
                : 0;
        }
    }
}

RasResult core_script_map_gridmap(LarNode* gridmap_exp, RasSceneGridMap* gridmap)
{

    memset(gridmap, 0, sizeof(RasSceneGridMap));
    gridmap->height = 1;

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

    for (size_t r = 0; r < gridmap->height; r++) {
        LarNode* row_node = &cells_node->list.nodes[r];

        for (size_t c = 0; c < gridmap->width; c++) {
            size_t dst_index = (r * gridmap->width) + c;
            LarNode* cell_node = &row_node->list.nodes[c];
            gridmap->cells[dst_index].material = cell_node->atom.val_integer;
        }
    }

    core_set_gridmap_cell_flags(gridmap);

    if (core_gridmap_to_pipeline_element(gridmap, &gridmap->element) != RAS_RESULT_OK) {
        ras_log_error("Failed to convert gridmap to pipeline element.");
        return RAS_RESULT_ERROR;
    }

    return RAS_RESULT_OK;
}
