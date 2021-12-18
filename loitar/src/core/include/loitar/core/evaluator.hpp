#pragma once

#include "list_node.hpp"
#include "node.hpp"
#include "result.hpp"
#include "spdlog/spdlog.h"
#include <memory>

namespace loitar {
typedef Result<std::vector<std::shared_ptr<Node>>> EvaluatorResult;
typedef Result<std::shared_ptr<Node>> EvaluatorNodeResult;

EvaluatorNodeResult evaluate_list_node(std::shared_ptr<ListNode> node);

EvaluatorResult evaluate_expression(
    std::vector<std::shared_ptr<Node>> expression, int depth);

EvaluatorResult evaluate(std::vector<std::shared_ptr<Node>> expressions);
}
