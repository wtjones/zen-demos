#pragma once

#include "environment.hpp"
#include "evaluator_types.hpp"
#include "integer_node.hpp"
#include "spdlog/spdlog.h"
#include "string_node.hpp"

namespace loitar {

void apply_syslib(Environment& env);
Function operator_add(Environment& env);
Function print(Environment& env);

}
