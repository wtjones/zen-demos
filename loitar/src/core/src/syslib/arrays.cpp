#include "loitar/core/syslib/arrays.hpp"
#include "loitar/core/array_node.hpp"
#include "loitar/core/evaluator.hpp"
#include "loitar/core/list_node.hpp"
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

namespace loitar::syslib {

Function make_array(Environment& env)
{
    Function func {
        .name = "make-array",
        .eval_params = true,
        .body = [&env](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.make_array with {} params", params.size());
            EvaluatorNodeResult result { .value = std::make_shared<NilNode>() };

            if (params.size() == 0) {
                ResultMessage message { .level = error, .message = "Expected params but received " + std::to_string(params.size()) };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }
            std::vector<size_t> dimensions;
            if (params.front()->name() == "IntegerNode") {
                dimensions.push_back(std::any_cast<int64_t>(params.front()->value()));
            } else {
                for (auto node : params.front()->get_elements()) {
                    dimensions.push_back(std::any_cast<int64_t>(node->value()));
                }
            }
            result.value = std::make_shared<ArrayNode>(dimensions);
            return result;
        }
    };
    return func;
}

void apply_syslib_arrays(Environment& env)
{
    env.add_function(make_array(env));
}
}
