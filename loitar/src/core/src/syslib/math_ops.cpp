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

Function operator_multiply(Environment& env)
{
    OpFunc op([](std::vector<std::any> values) -> std::shared_ptr<Node> {
        int64_t result = std::any_cast<int64_t>(values[0]);

        for (auto i = values.begin() + 1; i != values.end(); ++i) {
            result *= std::any_cast<int64_t>(*i);
        }
        return std::make_shared<IntegerNode>("", result);
    });

    return operator_func(env, op, "*");
}

Function operator_mod(Environment& env)
{
    Function func {
        .name = "mod",
        .eval_params = true,
        .body = [&env](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.mod with {} params", params.size());
            EvaluatorNodeResult result { .value = nullptr };

            if (params.size() != 2) {
                ResultMessage message { .level = error, .message = "Expected 2 param but received " + std::to_string(params.size()) };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            if (params.front()->name() != "IntegerNode" || params.back()->name() != "IntegerNode") {
                ResultMessage message { .level = error, .message = "Expected a number but received " + params.front()->name() };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            int64_t op_result = std::any_cast<int64_t>(params[0]->value()) % std::any_cast<int64_t>(params[1]->value());
            ;
            result.value = std::make_shared<IntegerNode>("", op_result);
            return result;
        }
    };
    return func;
}

void apply_syslib_math_ops(Environment& env)
{
    env.add_function(operator_add(env));
    env.add_function(operator_subtract(env));
    env.add_function(operator_multiply(env));
    env.add_function(operator_mod(env));
}

}
