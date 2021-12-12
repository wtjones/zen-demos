
#include "loitar/core/evaluator.hpp"
#include "loitar/core/integer_node.hpp"
#include <memory>
#include <typeinfo>

namespace loitar {

EvaluatorNodeResult evaluate_integer_node(std::shared_ptr<IntegerNode> node)
{
    EvaluatorNodeResult result { .value = node };
    return result;
}

EvaluatorResult evaluate_expression(
    std::vector<std::shared_ptr<Node>> expression, int depth)
{
    EvaluatorResult result;

    for (auto node : expression) {
        spdlog::trace("Checking name of {}", node->name());

        if (node->name() == "IntegerNode") {
            auto int_node = std::dynamic_pointer_cast<IntegerNode>(node);
            spdlog::trace("Eval int node");
            auto eval_result = evaluate_integer_node(int_node);
            result.value.push_back(eval_result.value);
        }
    }

    return result;
}

EvaluatorResult evaluate(std::vector<std::shared_ptr<Node>> expression)
{
    return evaluate_expression(expression, 0);
}
}
