#pragma once

#include "node.hpp"
#include "result.hpp"
#include "spdlog/spdlog.h"
#include <memory>

namespace loitar {
typedef Result<std::vector<std::shared_ptr<Node>>> EvaluatorResult;
typedef Result<std::shared_ptr<Node>> EvaluatorNodeResult;

EvaluatorResult evaluate(std::vector<std::shared_ptr<Node>> expressions);
}
