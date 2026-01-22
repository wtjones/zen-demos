#include "rasgl/core/debug.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/gridmap.h"

static inline void add_cell_face(
    RasPipelineElement* element,
    uint32_t v0i,
    uint32_t v1i,
    uint32_t v2i,
    uint32_t material_index)
{
    element->indexes[element->num_indexes++] = v0i;
    element->indexes[element->num_indexes++] = v1i;
    element->indexes[element->num_indexes++] = v2i;
    element->material_indexes[element->num_material_indexes++] = material_index;

    element->faces[element->num_faces].material_index = material_index;
    element->faces[element->num_faces].outline_edges = 0;
    element->faces[element->num_faces].outline_material_index = 0;
    core_face_normal(
        &element->verts[v0i].position,
        &element->verts[v1i].position,
        &element->verts[v2i].position,
        &element->faces[element->num_faces].normal);
    element->num_faces++;
}

RasResult core_gridmap_to_element_verts(
    RasSceneGridMap* gridmap,
    RasPipelineElement* element)
{
    if (element->max_verts == 0) {
        ras_log_error("Pipeline element not allocated for gridmap verts.");
        return RAS_RESULT_ERROR;
    }

    // Gennerate extra y vert for floor and ceiling.
    for (size_t y = 0; y < gridmap->height + 1; y++) {
        for (size_t z = 0; z < gridmap->depth + 1; z++) {
            for (size_t x = 0; x < gridmap->width + 1; x++) {
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

    core_get_element_aabb(element, &element->aabb);

    return RAS_RESULT_OK;
}

RasResult core_gridmap_to_element_verts_alloc(
    RasSceneGridMap* gridmap,
    RasPipelineElement* element)
{
    if (gridmap->height != 1) {
        ras_log_error("Only gridmaps with height 1 are supported.");
        return RAS_RESULT_ERROR;
    }

    return core_pipeline_element_alloc(
        (gridmap->width + 1) * (gridmap->depth + 1) * (gridmap->height + 1),
        gridmap->width * gridmap->depth * 8,
        gridmap->width * gridmap->depth * 24,
        gridmap->width * gridmap->depth * 8,
        element);
}

RasResult core_gridmap_to_element_faces(
    RasSceneGridMap* gridmap,
    RasCamera* camera,
    RasPipelineElement* element)
{

    if (gridmap->width * gridmap->depth * 8 > MAX_PIPELINE_FACES) {
        ras_log_error("Reached max faces in pipeline element: %d", MAX_PIPELINE_FACES);
        assert(false);
    }

    if (gridmap->width * gridmap->depth * 24 > MAX_VISIBLE_INDEXES) {
        ras_log_error("Reached max indexes in pipeline element: %d", MAX_VISIBLE_INDEXES);
        assert(false);
    }

    element->num_indexes = 0;
    element->num_faces = 0;
    element->num_material_indexes = 0;

    // Track the far-back-left and far-top-left vert indexes
    size_t fbl_index = 0;

    size_t z_cell = 0, x_cell = 0;
    size_t major_i = 0;
    size_t minor_i = 0;
    int32_t major_axis; // 0 = x, 1 = z
    int32_t major_dir;  // 0 = 0..len, 1 = len..0
    int32_t minor_dir;  // 0 = 0..len, 1 = len..0

    /*
        Camera angles from top-down:

                -Z
           225  180  135
                 |
        -X 270 --+-- 90 +X
                 |
           315   0   45
                +Z
    */

    RasOctant octant = core_angle_to_octant(camera->angle);

    switch (octant) {
    case DIR_N:
        // Looking down -Z, toward -X
        major_axis = 1;
        major_dir = 0;
        minor_dir = 0;
        ras_log_buffer("Looking down -Z");
        break;
    case DIR_NW:
        // Looking down -X, toward -Z
        major_axis = 0;
        major_dir = 0;
        minor_dir = 0;
        ras_log_buffer("Looking down -X");
        break;
    case DIR_W:
        // Looking down -X, toward +Z
        major_axis = 0;
        major_dir = 0;
        minor_dir = 1;
        ras_log_buffer("Looking down -X");
        break;
    case DIR_SW:
        // Looking down +Z, toward -X
        major_axis = 1;
        major_dir = 1;
        minor_dir = 0;
        ras_log_buffer("Looking down -X");
        break;
    case DIR_S:
        // Looking down +Z, toward +X
        major_axis = 1;
        major_dir = 1;
        minor_dir = 1;
        ras_log_buffer("Looking down +Z");
        break;
    case DIR_SE:
        // Looking down +X, toward +Z
        major_axis = 0;
        major_dir = 1;
        minor_dir = 1;
        ras_log_buffer("Looking down +X");
        break;
    case DIR_E:
        // Looking down +X, toward -Z
        major_axis = 0;
        major_dir = 1;
        minor_dir = 0;
        ras_log_buffer("Looking down +X");
        break;
    case DIR_NE:
        // Looking down -Z, toward +X
        major_axis = 1;
        major_dir = 0;
        minor_dir = 1;
        ras_log_buffer("Looking down -Z");
        break;
    default:
        ras_log_error("Invalid octant %d", octant);
        return RAS_RESULT_ERROR;
    }

    const size_t major_length = major_axis == 0 ? gridmap->width : gridmap->depth;
    const size_t minor_length = major_axis == 0 ? gridmap->depth : gridmap->width;
    for (major_i = 0; major_i < major_length; major_i++) {
        for (minor_i = 0; minor_i < minor_length; minor_i++) {
            if (major_axis == 0) {
                x_cell = major_dir == 0 ? major_i : (major_length - 1 - major_i);
                z_cell = minor_dir == 0 ? minor_i : (minor_length - 1 - minor_i);
            } else {
                z_cell = major_dir == 0 ? major_i : (major_length - 1 - major_i);
                x_cell = minor_dir == 0 ? minor_i : (minor_length - 1 - minor_i);
            }

            size_t cell_index = (z_cell * gridmap->width) + x_cell;
            RasGridMapCell* cell = &gridmap->cells[cell_index];

            fbl_index = (z_cell * (gridmap->width + 1)) + x_cell;
            size_t fbr_index = fbl_index + 1;
            size_t nbl_index = fbl_index + (gridmap->width + 1);
            size_t nbr_index = nbl_index + 1;

            size_t ftl_index = fbl_index + ((gridmap->width + 1) * (gridmap->depth + 1));
            size_t ftr_index = ftl_index + 1;
            size_t ntl_index = ftl_index + (gridmap->width + 1);
            size_t ntr_index = ntl_index + 1;
            if (cell->material > 0) {
                fbl_index++;
                continue; // wall
            }

            // Create interior floor face in CCW order.
            // Looking down:
            // FBL -- FBR
            // |       |
            // |       |
            // NBL -- NBR
            // Order: (FBR, FBL, NBL), (FBR, NBL, NBR)
            uint32_t checker = (z_cell + x_cell) % 2 == 0
                ? cell->material
                : (cell->material + 3);
            add_cell_face(element, fbr_index, fbl_index, nbl_index, checker);
            add_cell_face(element, fbr_index, nbl_index, nbr_index, checker);

            // Create interior ceiling face in CCW order.
            // Looking up:
            // NTL -- NTR
            // |       |
            // |       |
            // FTL -- FTR
            // Order: (NTR, NTL, FTL), (NTR, FTL, FTR)

            add_cell_face(element, ntr_index, ntl_index, ftl_index, checker);
            add_cell_face(element, ntr_index, ftl_index, ftr_index, checker);

            if (cell->spatial_flags & (1 << RAS_GRIDMAP_Z_MINUS_1)) {
                // Create interior far face in CCW order.
                // Looking far:
                // FTL -- FTR
                // |       |
                // |       |
                // FBL -- FBR
                // Order: (FTR, FTL, FBL), (FTR, FBL, FBR)

                add_cell_face(element,
                    ftr_index,
                    ftl_index,
                    fbl_index,
                    cell->spatial_materials[RAS_GRIDMAP_Z_MINUS_1]);
                add_cell_face(element,
                    ftr_index,
                    fbl_index,
                    fbr_index,
                    cell->spatial_materials[RAS_GRIDMAP_Z_MINUS_1]);
            }

            if (cell->spatial_flags & (1 << RAS_GRIDMAP_X_MINUS_1)) {
                // Create interior left face in CCW order.
                // Looking left:
                // NTL -- FTL
                // |       |
                // |       |
                // NBL -- FBL
                // Order: (FTL, NTL, NBL), (FTL, NBL, FBL)

                add_cell_face(
                    element,
                    ftl_index,
                    ntl_index,
                    nbl_index,
                    cell->spatial_materials[RAS_GRIDMAP_X_MINUS_1]);
                add_cell_face(
                    element,
                    ftl_index,
                    nbl_index,
                    fbl_index,
                    cell->spatial_materials[RAS_GRIDMAP_X_MINUS_1]);
            }

            if (cell->spatial_flags & (1 << RAS_GRIDMAP_Z_PLUS_1)) {
                // Create interior near face in CCW order.
                // Looking near:
                // NTR -- NTL
                // |       |
                // |       |
                // NBR -- NBL
                // Order: (NTL, NTR, NBR), (NTL, NBR, NBL)

                add_cell_face(element,
                    ntl_index,
                    ntr_index,
                    nbr_index,
                    cell->spatial_materials[RAS_GRIDMAP_Z_PLUS_1]);
                add_cell_face(element,
                    ntl_index,
                    nbr_index,
                    nbl_index,
                    cell->spatial_materials[RAS_GRIDMAP_Z_PLUS_1]);
            }

            if (cell->spatial_flags & (1 << RAS_GRIDMAP_X_PLUS_1)) {
                // Create interior right face in CCW order.
                // Looking right:
                // FTR -- NTR
                // |       |
                // |       |
                // FBR -- NBR
                // Order: (NTR, FTR, FBR), (NTR, FBR, NBR)

                add_cell_face(
                    element,
                    ntr_index,
                    ftr_index,
                    fbr_index,
                    cell->spatial_materials[RAS_GRIDMAP_X_PLUS_1]);
                add_cell_face(
                    element,
                    ntr_index,
                    fbr_index,
                    nbr_index,
                    cell->spatial_materials[RAS_GRIDMAP_X_PLUS_1]);
            }
            fbl_index++;
        }
        fbl_index++;
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

            cell->spatial_materials[RAS_GRIDMAP_Z_MINUS_1] = cell_z_minus != NULL ? cell_z_minus->material : -1;
            cell->spatial_materials[RAS_GRIDMAP_Z_PLUS_1] = cell_z_plus != NULL ? cell_z_plus->material : -1;
            cell->spatial_materials[RAS_GRIDMAP_X_MINUS_1] = cell_x_minus != NULL ? cell_x_minus->material : -1;
            cell->spatial_materials[RAS_GRIDMAP_X_PLUS_1] = cell_x_plus != NULL ? cell_x_plus->material : -1;

            cell->spatial_flags = cell_z_minus != NULL && cell_z_minus->material > 0
                ? 1 << RAS_GRIDMAP_Z_MINUS_1
                : 0;
            cell->spatial_flags |= cell_z_plus != NULL && cell_z_plus->material > 0
                ? 1 << RAS_GRIDMAP_Z_PLUS_1
                : 0;
            cell->spatial_flags |= cell_x_minus != NULL && cell_x_minus->material > 0
                ? 1 << RAS_GRIDMAP_X_MINUS_1
                : 0;
            cell->spatial_flags |= cell_x_plus != NULL && cell_x_plus->material > 0
                ? 1 << RAS_GRIDMAP_X_PLUS_1
                : 0;
        }
    }
}
