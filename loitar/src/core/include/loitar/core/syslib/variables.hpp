#pragma once

#include "../environment.hpp"
#include "../evaluator_types.hpp"
#include "../integer_node.hpp"
#include "../nil_node.hpp"
#include "../string_node.hpp"
#include "../true_node.hpp"
#include "spdlog/spdlog.h"

namespace loitar::syslib {

void apply_syslib_variables(Environment& env);
}
