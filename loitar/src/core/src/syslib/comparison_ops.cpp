#include "loitar/core/syslib/comparison_ops.hpp"
#include "loitar/core/evaluator.hpp"
#include "loitar/core/list_node.hpp"
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

namespace loitar::syslib {

Function operator_eq(Environment& env)
{
    Function func {
        .name = "eq",
        .eval_params = true,
        .body = [env](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.operator_eq with {} params", params.size());
            EvaluatorNodeResult result { .value = nullptr };

            if (params.size() != 2) {
                ResultMessage message { .level = error, .message = "Expected 2 params but received " + std::to_string(params.size()) };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            if (*(params.front()) == *(params.back())) {
                result.value = std::make_shared<TrueNode>();
            } else {
                result.value = std::make_shared<NilNode>();
            }

            spdlog::trace("operator_eq result with {}", result.value->name());
            return result;
        }
    };
    return func;
}

void apply_syslib_comparison_ops(Environment& env)
{
    env.add_function(operator_eq(env));
}

}
