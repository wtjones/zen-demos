
#include "rasgl/core/scene.h"

void core_update_animation(RasSceneObject* scene_object)
{
    if (!scene_object->animation) {
        return;
    }
    RasSceneObjectAnimationRotation* rotation = &scene_object->animation->rotation;

    RasVector3f* model_rotation = &scene_object->rotation;

    model_rotation->x = rotation->axis.x == RAS_FIXED_ONE
        ? (model_rotation->x + rotation->speed) % INT_32_TO_FIXED_16_16(360)
        : model_rotation->x;

    if (model_rotation->x < 0) {
        model_rotation->x += INT_32_TO_FIXED_16_16(360);
    }

    model_rotation->y = rotation->axis.y == RAS_FIXED_ONE
        ? (model_rotation->y + rotation->speed) % INT_32_TO_FIXED_16_16(360)
        : model_rotation->y;

    if (model_rotation->y < 0) {
        model_rotation->y += INT_32_TO_FIXED_16_16(360);
    }

    model_rotation->z = rotation->axis.z == RAS_FIXED_ONE
        ? (model_rotation->z + rotation->speed) % INT_32_TO_FIXED_16_16(360)
        : model_rotation->z;

    if (model_rotation->z < 0) {
        model_rotation->z += INT_32_TO_FIXED_16_16(360);
    }
}
