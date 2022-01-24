#include "loitar/core/syslib/comparison_ops.hpp"
#include "loitar/core/evaluator.hpp"
#include "loitar/core/list_node.hpp"
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

namespace loitar::syslib {

typedef std::function<bool(Node&, Node&)> OpFunc;

Function operator_func(Environment& env, OpFunc op, std::string name)
{
    Function func {
        .name = name,
        .eval_params = true,
        .body = [name, op](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.operator_eq with {} params", params.size());
            EvaluatorNodeResult result { .value = nullptr };

            if (params.size() != 2) {
                ResultMessage message { .level = error, .message = "Expected 2 params but received " + std::to_string(params.size()) };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            auto a = params.front();
            auto b = params.back();

            if (op(*a, *b)) {
                result.value = std::make_shared<TrueNode>();
            } else {
                result.value = std::make_shared<NilNode>();
            }

            spdlog::trace("operator_func {} result with {}", name, result.value->name());
            return result;
        }
    };
    return func;
}

Function operator_eq(Environment& env)
{
    OpFunc op([](Node& a, Node& b) -> bool {
        return a == b;
    });
    return operator_func(env, op, "=");
}

Function operator_not_eq(Environment& env)
{
    OpFunc op([](Node& a, Node& b) -> bool {
        return a != b;
    });
    return operator_func(env, op, "/=");
}

Function operator_lt(Environment& env)
{
    OpFunc op([](Node& a, Node& b) -> bool {
        return a < b;
    });
    return operator_func(env, op, "<");
}

Function operator_lt_eq(Environment& env)
{
    OpFunc op([](Node& a, Node& b) -> bool {
        return a <= b;
    });
    return operator_func(env, op, "<=");
}

Function operator_gt(Environment& env)
{
    OpFunc op([](Node& a, Node& b) -> bool {
        return a > b;
    });
    return operator_func(env, op, ">");
}

Function operator_gt_eq(Environment& env)
{
    OpFunc op([](Node& a, Node& b) -> bool {
        return a >= b;
    });
    return operator_func(env, op, ">=");
}

void apply_syslib_comparison_ops(Environment& env)
{
    env.add_function(operator_eq(env));
    env.add_function(operator_not_eq(env));
    env.add_function(operator_lt(env));
    env.add_function(operator_lt_eq(env));
    env.add_function(operator_gt(env));
    env.add_function(operator_gt_eq(env));
}

}
