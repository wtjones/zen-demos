#pragma once

#include "environment.hpp"
#include "evaluator_types.hpp"
#include "list_node.hpp"
#include "node.hpp"
#include "result.hpp"
#include "spdlog/spdlog.h"
#include <memory>

namespace loitar {

EvaluatorNodeResult evaluate_list_node(Environment& env, std::shared_ptr<ListNode> node);

EvaluatorResult evaluate_expression(
    Environment& env,
    std::vector<std::shared_ptr<Node>> expression,
    int depth);

EvaluatorResult evaluate(Environment& env, std::vector<std::shared_ptr<Node>> expressions);
}
