#include "loitar/core/syslib/math_ops.hpp"
#include "loitar/core/evaluator.hpp"
#include "loitar/core/list_node.hpp"
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

namespace loitar::syslib {

Function operator_add(Environment& env)
{
    Function func {
        .name = "+",
        .eval_params = true,
        .body = [env](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.operator_add with {} params", params.size());
            EvaluatorNodeResult result { .value = nullptr };
            auto sum = 0;
            for (auto node : params) {
                if (node->name() == "IntegerNode") {
                    auto int_node = std::any_cast<int64_t>(node->value());
                    spdlog::trace("Summing {}", int_node);
                    sum += int_node;
                } else {
                    ResultMessage message { .level = error, .message = "Expected IntegerNode but received " + node->name() };
                    result.messages.push_back(message);
                    spdlog::info("{}", message.message);
                    return result;
                }
            }
            result.value = std::make_shared<IntegerNode>("", sum);
            spdlog::trace("operator_add result with {}", sum);
            return result;
        }
    };
    return func;
}

void apply_syslib_math_ops(Environment& env)
{
    env.add_function(operator_add(env));
}

}
