#pragma once

#include "environment.hpp"
#include "evaluator_types.hpp"
#include "integer_node.hpp"
#include "nil_node.hpp"
#include "spdlog/spdlog.h"
#include "string_node.hpp"
#include "true_node.hpp"

namespace loitar {

void apply_syslib(Environment& env);

}
