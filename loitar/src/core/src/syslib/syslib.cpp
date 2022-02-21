#include "loitar/core/syslib/syslib.hpp"
#include "loitar/core/syslib/arrays.hpp"
#include "loitar/core/syslib/comparison_ops.hpp"
#include "loitar/core/syslib/flow_control.hpp"
#include "loitar/core/syslib/functions.hpp"
#include "loitar/core/syslib/input_output.hpp"
#include "loitar/core/syslib/lists.hpp"
#include "loitar/core/syslib/math_ops.hpp"
#include "loitar/core/syslib/predicates.hpp"
#include "loitar/core/syslib/special_ops.hpp"
#include "loitar/core/syslib/variables.hpp"

namespace loitar::syslib {

void apply_syslib(Environment& env)
{
    apply_syslib_arrays(env);
    apply_syslib_comparison_ops(env);
    apply_syslib_flow_control(env);
    apply_syslib_functions(env);
    apply_syslib_input_output(env);
    apply_syslib_lists(env);
    apply_syslib_math_ops(env);
    apply_syslib_predicates(env);
    apply_syslib_special_ops(env);
    apply_syslib_variables(env);
}

}
