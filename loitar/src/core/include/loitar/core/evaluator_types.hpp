#pragma once

#include "list_node.hpp"
#include "node.hpp"
#include "result.hpp"
#include <memory>

namespace loitar {
typedef Result<std::vector<std::shared_ptr<Node>>> EvaluatorResult;
typedef Result<std::shared_ptr<Node>> EvaluatorNodeResult;

}
