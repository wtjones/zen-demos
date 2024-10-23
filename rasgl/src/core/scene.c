#include "rasgl/core/scene.h"
#include "rasgl/core/graphics.h"
#include "rasgl/core/model.h"
#include <string.h>

RasResult core_script_map_int32(LarNode* exp, const char* symbol, int32_t* dest)
{
    LarNode* node = lar_get_property_by_type(exp, symbol, LAR_NODE_ATOM_INTEGER);

    RAS_CHECK_AND_LOG(node == NULL,
        "Failed to find property %s", symbol);

    *dest = node->atom.val_integer;
    return RAS_RESULT_OK;
}

RasResult core_script_map_vector(LarNode* vector_exp, RasVector3f* dest)
{
    RAS_CHECK_AND_LOG(!lar_is_symbol(lar_get_first(vector_exp), "vec"),
        "Vector must have a symbol")

    LarNode* x = lar_get_list_node_by_index(vector_exp, 1);
    LarNode* y = lar_get_list_node_by_index(vector_exp, 2);
    LarNode* z = lar_get_list_node_by_index(vector_exp, 3);

    if (x == NULL || y == NULL || z == NULL) {
        ras_log_error("Vector must have 3 components");
        return RAS_RESULT_ERROR;
    }

    RAS_CHECK_AND_LOG(x == NULL || y == NULL || z == NULL,
        "Vector must have 3 components");

    dest->x = x->atom.val_fixed;
    dest->y = y->atom.val_fixed;
    dest->z = z->atom.val_fixed;

    return RAS_RESULT_OK;
}

RasResult core_script_map_camera(RasScene* scene, LarNode* camera_exp, RasCamera* camera)
{
    LarNode* position = lar_get_property_by_type(
        camera_exp, SCRIPT_SYMBOL_POSITION, LAR_NODE_LIST);

    RAS_CHECK_AND_LOG(position == NULL,
        "Failed to find property %s", SCRIPT_SYMBOL_POSITION);

    RasResult result = core_script_map_vector(position, &camera->position);

    RAS_CHECK_RESULT_AND_LOG(result,
        "Failed to map property %s", SCRIPT_SYMBOL_POSITION);

    result = core_script_map_int32(
        camera_exp, SCRIPT_SYMBOL_ANGLE, &camera->angle);

    RAS_CHECK_RESULT_AND_LOG(result,
        "Failed to map property %s", SCRIPT_SYMBOL_ANGLE);

    return RAS_RESULT_OK;
}

RasResult core_script_map_cameras(LarNode* scene_exp, RasScene* scene)
{
    LarNode* cameras_list = lar_get_list_by_symbol(
        scene_exp, SCRIPT_SYMBOL_CAMERAS);

    RAS_CHECK_AND_LOG(cameras_list == NULL,
        "Objects list not found in script");

    size_t num_cameras = cameras_list->list.length - 1; // exclude the symbol

    RasCamera* cameras = (RasCamera*)malloc(
        sizeof(RasCamera) * num_cameras);

    RAS_CHECK_AND_LOG(cameras == NULL,
        "Failed to allocate memory for cameras");

    for (size_t i = 0; i < num_cameras; i++) {
        // skip the symbol
        LarNode* item_exp = lar_get_list_node_by_index(cameras_list, i + 1);

        RasCamera* camera = &cameras[i];

        RasResult result = core_script_map_camera(scene, item_exp, camera);

        if (result != RAS_RESULT_OK) {
            ras_log_error("Failed to map camera");
            free(cameras);
            return RAS_RESULT_ERROR;
        }
    }

    scene->cameras = cameras;
    scene->num_cameras = num_cameras;

    return RAS_RESULT_OK;
}

RasResult core_script_map_object(
    RasScene* scene, LarNode* object_exp, RasSceneObject* scene_object)
{

    LarNode* model_name = lar_get_property_by_type(
        object_exp, SCRIPT_SYMBOL_MODEL_NAME, LAR_NODE_ATOM_STRING);

    RAS_CHECK_AND_LOG(model_name == NULL,
        "Failed to find property %s", SCRIPT_SYMBOL_MODEL_NAME);

    // Look up the model by name and assign the reference
    scene_object->element_ref = NULL;
    for (size_t i = 0; i < scene->num_models; i++) {
        RasSceneModel* model = &scene->models[i];
        if (strcmp(model->name, model_name->atom.val_symbol) == 0) {
            scene_object->element_ref = &model->element;
            break;
        }
    }

    RAS_CHECK_AND_LOG(scene_object->element_ref == NULL,
        "Model %s referenced by object not found", model_name->atom.val_symbol);

    LarNode* position = lar_get_property_by_type(
        object_exp, SCRIPT_SYMBOL_POSITION, LAR_NODE_LIST);

    RAS_CHECK_AND_LOG(position == NULL,
        "Failed to find property %s", SCRIPT_SYMBOL_POSITION);

    // Set initial rotation based on orientation
    RasResult result = core_script_map_vector(position, &scene_object->position);

    RAS_CHECK_RESULT_AND_LOG(result,
        "Failed to map property %s", SCRIPT_SYMBOL_POSITION);

    LarNode* orientation = lar_get_property_by_type(
        object_exp, SCRIPT_SYMBOL_ORIENTATION, LAR_NODE_LIST);

    RAS_CHECK_AND_LOG(orientation == NULL,
        "Failed to find property %s", SCRIPT_SYMBOL_ORIENTATION);

    // Set initial rotation based on orientation
    result = core_script_map_vector(orientation, &scene_object->rotation);

    RAS_CHECK_RESULT_AND_LOG(result,
        "Failed to map property %s", SCRIPT_SYMBOL_ORIENTATION);

    return RAS_RESULT_OK;
}

RasResult core_script_map_objects(LarNode* scene_exp, RasScene* scene)
{
    LarNode* objects_list = lar_get_list_by_symbol(
        scene_exp, SCRIPT_SYMBOL_OBJECTS);

    RAS_CHECK_AND_LOG(objects_list == NULL,
        "Objects list not found in script");

    size_t num_objects = objects_list->list.length - 1; // exclude the symbol

    RasSceneObject* objects = (RasSceneObject*)malloc(
        sizeof(RasSceneObject) * num_objects);

    RAS_CHECK_AND_LOG(objects == NULL,
        "Failed to allocate memory for scene objects");

    for (size_t i = 0; i < num_objects; i++) {
        // skip the symbol
        LarNode* object_exp = lar_get_list_node_by_index(objects_list, i + 1);

        RasSceneObject* object = &objects[i];

        RasResult result = core_script_map_object(scene, object_exp, object);

        if (result != RAS_RESULT_OK) {
            ras_log_error("Failed to map object");
            free(objects);
            return RAS_RESULT_ERROR;
        }
    }

    scene->objects = objects;
    scene->num_objects = num_objects;

    return RAS_RESULT_OK;
}

RasResult core_script_map_name(LarNode* scene_exp, RasScene* scene)
{

    LarNode* scene_name = lar_get_property_by_type(
        scene_exp, SCRIPT_SYMBOL_NAME, LAR_NODE_ATOM_STRING);

    RAS_CHECK_AND_LOG(scene_name == NULL,
        "Scene %s is required", SCRIPT_SYMBOL_NAME);

    strcpy(scene->name, scene_name->atom.val_string);
    return RAS_RESULT_OK;
}

/**
 * @brief With the given script model, load the RasModel and
 * apply to the RasSceneModel
 *
 * @param model_exp
 * @param model must be freed via core_free_scene_model()
 * @return RasResult
 */
RasResult core_script_map_model(LarNode* model_exp, RasSceneModel* scene_model)
{
    LarNode* model_name = lar_get_property_by_type(
        model_exp, SCRIPT_SYMBOL_NAME, LAR_NODE_ATOM_STRING);

    RAS_CHECK_AND_LOG(model_name == NULL,
        "Property %s is required", SCRIPT_SYMBOL_NAME);

    strcpy(scene_model->name, model_name->atom.val_string);

    LarNode* model_path = lar_get_property_by_type(
        model_exp, SCRIPT_SYMBOL_PATH, LAR_NODE_ATOM_STRING);

    RAS_CHECK_AND_LOG(model_path == NULL,
        "Property %s is required", SCRIPT_SYMBOL_PATH);

    strcpy(scene_model->path, model_path->atom.val_string);
    ras_log_info("Mapped model %s to %s", scene_model->name, scene_model->path);

    RasModel* file_model = core_load_model(scene_model->path);

    if (file_model == NULL) {
        ras_log_error("Failed to load model %s", scene_model->path);
        free(file_model);
        return RAS_RESULT_ERROR;
    }

    core_model_group_to_pipeline_element(
        &file_model->groups[0], &scene_model->element);
    ras_log_info("Mapped model %s to pipeline elements", scene_model->name);

    core_free_model(file_model);
    return RAS_RESULT_OK;
}

RasResult core_script_map_models(LarNode* scene_exp, RasScene* scene)
{
    LarNode* models_list = lar_get_list_by_symbol(
        scene_exp, SCRIPT_SYMBOL_MODELS);
    RAS_CHECK_AND_LOG(models_list == NULL,
        "Models list not found in script");

    size_t num_models = models_list->list.length - 1; // exclude the symbol

    RasSceneModel* models = (RasSceneModel*)malloc(
        sizeof(RasSceneModel) * num_models);

    RAS_CHECK_AND_LOG(models == NULL,
        "Failed to allocate memory for models");

    for (size_t i = 0; i < num_models; i++) {
        // skip the symbol
        LarNode* model_exp = lar_get_list_node_by_index(models_list, i + 1);

        RasSceneModel* model = &models[i];

        RasResult result = core_script_map_model(model_exp, model);

        if (result != RAS_RESULT_OK) {
            ras_log_error("Failed to map model");
            free(models);
            return RAS_RESULT_ERROR;
        }
    }

    scene->models = models;
    scene->num_models = num_models;

    return RAS_RESULT_OK;
}

RasResult core_script_map_scene(LarScript* script, RasScene** scene)
{
    LarNode* scene_exp = lar_get_list_by_symbol(
        script->expressions, SCRIPT_SYMBOL_SCENE);

    RAS_CHECK_AND_LOG(scene_exp == NULL,
        "Scene expression not found in script");

    RasScene* new_scene = (RasScene*)malloc(sizeof(RasScene));

    RAS_CHECK_AND_LOG(new_scene == NULL,
        "Failed to allocate memory for scene");

    RasResult result = core_script_map_name(scene_exp, new_scene);
    if (result != RAS_RESULT_OK) {
        ras_log_error("Failed to map scene name");
        free(new_scene);
        return RAS_RESULT_ERROR;
    }

    result = core_script_map_models(scene_exp, new_scene);

    if (result != RAS_RESULT_OK) {
        ras_log_error("Failed to map models");
        free(new_scene);
        return RAS_RESULT_ERROR;
    }

    result = core_script_map_objects(scene_exp, new_scene);

    if (result != RAS_RESULT_OK) {
        ras_log_error("Failed to map objects");
        free(new_scene);
        return RAS_RESULT_ERROR;
    }

    result = core_script_map_cameras(scene_exp, new_scene);

    if (result != RAS_RESULT_OK) {
        ras_log_error("Failed to map cameras");
        free(new_scene);
        return RAS_RESULT_ERROR;
    }

    ras_log_info("Mapped scene %s", new_scene->name);
    *scene = new_scene;

    return RAS_RESULT_OK;
}

RasResult core_load_scene(const char* path, RasScene** scene)
{
    LarScript* script;
    LarParseResult result = lar_parse_file(path, &script);

    RAS_CHECK_AND_LOG(result != LAR_PARSE_RESULT_OK,
        "Failed to parse scene file: %s", path);

    char* repr = lar_repr_script(script);
    ras_log_info("Script: %s", repr);
    free(repr);

    result = core_script_map_scene(script, scene);

    if (result != RAS_RESULT_OK) {
        ras_log_error("Failed to map script to scene");
        lar_free_script(&script);

        return RAS_RESULT_ERROR;
    }
    lar_free_script(&script);

    return RAS_RESULT_OK;
}

void core_free_scene_models(RasScene* scene)
{
    // TODO: free RasModel
    free(scene->models);
}

void core_free_scene(RasScene** scene)
{
    RasScene* s = *scene;
    core_free_scene_models(s);
    free(s->objects);
    free(s);
    *scene = NULL;
}
