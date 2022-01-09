#include "loitar/core/syslib/variables.hpp"
#include "loitar/core/evaluator.hpp"
#include "loitar/core/list_node.hpp"
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

namespace loitar::syslib {

Function setq(Environment& env)
{
    Function func {
        .name = "setq",
        .eval_params = false,
        .body = [&env](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.setq  with {} params", params.size());
            EvaluatorNodeResult result { .value = nullptr };

            if (params.size() != 2) {
                ResultMessage message { .level = error, .message = "Expected 2 param but received " + std::to_string(params.size()) };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            if (params.front()->name() != "AtomNode") {
                ResultMessage message { .level = error, .message = "Expected AtomNode param but received " + params.front()->name() };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            auto var_name = std::dynamic_pointer_cast<AtomNode>(params.front())->get_token();

            std::vector<std::shared_ptr<Node>> eval_param;
            eval_param.push_back(params[1]);

            auto eval_result = evaluate_expression(env, eval_param, 0);
            if (eval_result.messages.size() > 0) {
                std::copy(eval_result.messages.begin(), eval_result.messages.end(), std::back_inserter(result.messages));
                return result;
            }
            auto var_value = eval_result.value.front();

            std::ostringstream ss;
            ss << *var_value;
            spdlog::trace("Setting variable {} {} {}", var_name, var_value->name(), ss.str());

            env.set_variable(var_name, var_value, ScopeType::global);
            result.value = var_value;
            return result;
        }
    };
    return func;
}

void apply_syslib_variables(Environment& env)
{
    env.add_function(setq(env));
}

}
