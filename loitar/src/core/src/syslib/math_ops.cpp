#include "loitar/core/syslib/math_ops.hpp"
#include "loitar/core/evaluator.hpp"
#include "loitar/core/list_node.hpp"
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

namespace loitar::syslib {

typedef std::function<std::shared_ptr<Node>(std::vector<std::any>)> OpFunc;

Function operator_func(Environment& env, OpFunc op, std::string name)
{
    Function func {
        .name = name,
        .eval_params = true,
        .body = [env, name, op](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.operator_fun {} with {} params", name, params.size());
            EvaluatorNodeResult result { .value = nullptr };
            auto sum = 0;
            std::vector<std::any> values;

            for (auto node : params) {
                if (node->name() == "IntegerNode") {
                    values.push_back(node->value());
                } else {
                    ResultMessage message { .level = error, .message = "Expected IntegerNode but received " + node->name() };
                    result.messages.push_back(message);
                    spdlog::info("{}", message.message);
                    return result;
                }
            }

            result.value = op(values);
            return result;
        }
    };
    return func;
}

Function operator_add(Environment& env)
{
    OpFunc op([](std::vector<std::any> values) -> std::shared_ptr<Node> {
        int64_t result = std::any_cast<int64_t>(values[0]);

        for (auto i = values.begin() + 1; i != values.end(); ++i) {
            result += std::any_cast<int64_t>(*i);
        }
        return std::make_shared<IntegerNode>("", result);
    });

    return operator_func(env, op, "+");
}

Function operator_subtract(Environment& env)
{
    OpFunc op([](std::vector<std::any> values) -> std::shared_ptr<Node> {
        int64_t result = std::any_cast<int64_t>(values[0]);

        for (auto i = values.begin() + 1; i != values.end(); ++i) {
            result -= std::any_cast<int64_t>(*i);
        }
        return std::make_shared<IntegerNode>("", result);
    });

    return operator_func(env, op, "-");
}

void apply_syslib_math_ops(Environment& env)
{
    env.add_function(operator_add(env));
    env.add_function(operator_subtract(env));
}

}
