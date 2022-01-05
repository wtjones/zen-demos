#include "loitar/core/syslib/input_output.hpp"
#include "loitar/core/evaluator.hpp"
#include "loitar/core/list_node.hpp"
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

namespace loitar::syslib {

Function print(Environment& env)
{
    Function func {
        .name = "print",
        .eval_params = true,
        .body = [&env](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.print  with {} params", params.size());
            EvaluatorNodeResult result { .value = nullptr };

            if (params.size() != 1) {
                ResultMessage message { .level = error, .message = "Expected 1 param but received " + std::to_string(params.size()) };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            std::ostringstream ss;
            ss << *(params.front());
            auto string_node = std::make_shared<StringNode>("", ss.str());
            result.value = string_node;
            std::cout << string_node->get_value() << std::endl;
            return result;
        }
    };
    return func;
}

void apply_syslib_input_output(Environment& env)
{
    env.add_function(print(env));
}

}
