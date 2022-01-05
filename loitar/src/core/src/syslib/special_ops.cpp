#include "loitar/core/evaluator.hpp"
#include "loitar/core/list_node.hpp"
#include "loitar/core/syslib/special_ops.hpp"
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

namespace loitar::syslib {

Function quote(Environment& env)
{
    Function func {
        .name = "quote",
        .eval_params = false,
        .body = [&env](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.quote with {} params", params.size());
            EvaluatorNodeResult result { .value = nullptr };

            if (params.size() != 1) {
                ResultMessage message { .level = error, .message = "Expected 1 param but received " + std::to_string(params.size()) };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            result.value = params.front();
            return result;
        }
    };
    return func;
}

void apply_syslib_special_ops(Environment& env)
{
    env.add_function(quote(env));
}

}
