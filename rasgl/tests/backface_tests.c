
#include "rasgl/core/debug.h"
#include "rasgl/core/repr.h"
#include "tests.h"

typedef struct RasVector3fp {
    float x;
    float y;
    float z;

} RasVector3fp;

/**
 * @brief Floating point test version for comparison.
 *
 * @param sv0
 * @param sv1
 * @param sv2
 * @return true
 * @return false
 */
bool core_is_backface_fp(RasVector3fp* sv0, RasVector3fp* sv1, RasVector3fp* sv2)
{
    // norm1 = (1.x - 0.x) * (0.y - 2.y)
    // norm2 = (1.y - 0.y) * (0.x - 2.x)
    float norm1 = (sv1->x - sv0->x) * (sv0->y - sv2->y);
    float norm2 = (sv1->y - sv0->y) * (sv0->x - sv2->x);

    float norm = norm1 - norm2;

    // Debug output for problematic cases

    return norm < 0;
}

void backface_tests()
{

    /*
    call first with result = false (not backface) example:

    [x: 86.677246, y: 22.238159, z: 0.948990, w: 1.700806]
[x: -29.384766, y: 296.057739, z: 0.901703, w: 0.944412]
[x: 131.284180, y: 187.666626, z: 0.967163, w: 2.457214]
    */

    RasVector3fp sv0 = {
        .x = 86.677246f,
        .y = 22.238159f,
        .z = 0.948990f
    };

    RasVector3fp sv1 = {
        .x = -29.384766f,
        .y = 296.057739f,
        .z = 0.901703f
    };

    RasVector3fp sv2 = {
        .x = 131.284180f,
        .y = 187.666626f,
        .z = 0.967163f
    };

    bool result = core_is_backface_fp(&sv0, &sv1, &sv2);

    assert(!result);
    /*

    call with result = true (is backface) expectation

    [x: 86.677246, y: 22.238159, z: 0.948990, w: 1.700806]
[x: -36.689453, y: 292.406616, z: 0.903915, w: 0.964417]
[x: 135.493164, y: 188.221436, z: 0.966827, w: 2.437210]
    */

    sv0.x = 86.677246f;
    sv0.y = 22.238159f;
    sv0.z = 0.948990f;

    sv1.x = -36.689453f;
    sv1.y = 292.406616f;
    sv1.z = 0.903915f;

    sv2.x = 135.493164f;
    sv2.y = 188.221436f;
    sv2.z = 0.966827f;

    result = core_is_backface_fp(&sv0, &sv1, &sv2);

    assert(!result);

    // Now test the same scenarios with the fixed-point version core_is_backface
    ras_log_info("Testing fixed-point core_is_backface...\n");

    // Test case 1: Front-facing triangle (should return false)
    RasVector4f sv0_fixed = {
        .x = float_to_fixed_16_16(86.677246f),
        .y = float_to_fixed_16_16(22.238159f),
        .z = float_to_fixed_16_16(0.948990f),
        .w = float_to_fixed_16_16(1.700806f)
    };

    RasVector4f sv1_fixed = {
        .x = float_to_fixed_16_16(-29.384766f),
        .y = float_to_fixed_16_16(296.057739f),
        .z = float_to_fixed_16_16(0.901703f),
        .w = float_to_fixed_16_16(0.944412f)
    };

    RasVector4f sv2_fixed = {
        .x = float_to_fixed_16_16(131.284180f),
        .y = float_to_fixed_16_16(187.666626f),
        .z = float_to_fixed_16_16(0.967163f),
        .w = float_to_fixed_16_16(2.457214f)
    };

    bool fixed_result = core_is_backface(&sv0_fixed, &sv1_fixed, &sv2_fixed);
    ras_log_info("Fixed-point test 1 (front-facing): %s\n", fixed_result ? "backface" : "front-facing");
    assert(!fixed_result);

    // Test case 2: Back-facing triangle (should return true)
    sv0_fixed.x = float_to_fixed_16_16(86.677246f);
    sv0_fixed.y = float_to_fixed_16_16(22.238159f);
    sv0_fixed.z = float_to_fixed_16_16(0.948990f);
    sv0_fixed.w = float_to_fixed_16_16(1.700806f);

    sv1_fixed.x = float_to_fixed_16_16(-36.689453f);
    sv1_fixed.y = float_to_fixed_16_16(292.406616f);
    sv1_fixed.z = float_to_fixed_16_16(0.903915f);
    sv1_fixed.w = float_to_fixed_16_16(0.964417f);

    sv2_fixed.x = float_to_fixed_16_16(135.493164f);
    sv2_fixed.y = float_to_fixed_16_16(188.221436f);
    sv2_fixed.z = float_to_fixed_16_16(0.966827f);
    sv2_fixed.w = float_to_fixed_16_16(2.437210f);

    fixed_result = core_is_backface(&sv0_fixed, &sv1_fixed, &sv2_fixed);

    ras_log_info("Fixed-point test 2 (back-facing): %s\n", fixed_result ? "backface" : "front-facing");
    assert(!fixed_result);
    ras_log_info("Fixed-point backface tests completed successfully!\n");
}

void fixed_sub_tests()
{
    RasFixed norm1 = 1341968150;
    RasFixed norm2 = -864322928;

    RasFixed result = norm1 - norm2;

    char buffer[255];
    ras_log_info(
        "Fixed-point subtraction test: %s\n", repr_fixed_16_16(buffer, sizeof(buffer), result));
    assert(result > 0);
}
