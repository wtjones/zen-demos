#include "loitar/core/syslib.hpp"
#include "loitar/core/evaluator.hpp"
#include "loitar/core/list_node.hpp"
#include <iostream>

namespace loitar {

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
                    auto int_node = std::dynamic_pointer_cast<IntegerNode>(node);
                    spdlog::trace("Summing {}", int_node->get_value());
                    sum += int_node->get_value();
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

Function operator_if(Environment& env)
{
    Function func {
        .name = "if",
        .eval_params = false,
        .body = [&env](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.operator_if with {} params", params.size());
            EvaluatorNodeResult result { .value = nullptr };

            if (params.size() <= 2 || params.size() > 3) {
                ResultMessage message { .level = error, .message = "Expected 2-3 params but received " + std::to_string(params.size()) };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            std::vector<std::shared_ptr<Node>> cond_param;
            cond_param.push_back(params.front());

            auto eval_result = evaluate_expression(env, cond_param, 0);
            if (eval_result.messages.size() > 0) {
                std::copy(eval_result.messages.begin(), eval_result.messages.end(), std::back_inserter(result.messages));
                return result;
            }

            if (*(eval_result.value.front()) == TrueNode()) {
                spdlog::trace("if condition is true");
                std::vector<std::shared_ptr<Node>> eval_param;
                eval_param.push_back(params[1]);

                auto eval_result = evaluate_expression(env, eval_param, 0);
                if (eval_result.messages.size() > 0) {
                    std::copy(eval_result.messages.begin(), eval_result.messages.end(), std::back_inserter(result.messages));
                    return result;
                }

                spdlog::trace("evaluated true expression...");
                if (eval_result.messages.size() > 0) {
                    spdlog::trace("evaluated true condition... bad");
                    std::copy(eval_result.messages.begin(), eval_result.messages.end(), std::back_inserter(result.messages));
                    return result;
                }
                result.value = eval_result.value.front();
            } else {
                spdlog::trace("if condition is false");
                if (params.size() == 2) {
                    result.value = std::make_shared<NilNode>();
                } else {
                    std::vector<std::shared_ptr<Node>> eval_param;
                    eval_param.push_back(params[2]);

                    auto eval_result = evaluate_expression(env, eval_param, 0);
                    if (eval_result.messages.size() > 0) {
                        std::copy(eval_result.messages.begin(), eval_result.messages.end(), std::back_inserter(result.messages));
                        return result;
                    }

                    spdlog::trace("evaluated false expression...");
                    if (eval_result.messages.size() > 0) {
                        spdlog::trace("evaluated false expression... bad");
                        std::copy(eval_result.messages.begin(), eval_result.messages.end(), std::back_inserter(result.messages));
                        return result;
                    }
                    result.value = eval_result.value.front();
                }
            }

            spdlog::trace("operator_eq result with {}", result.value->name());
            return result;
        }
    };
    return func;
}

Function print(Environment& env)
{
    Function func {
        .name = "print",
        .body = [env](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.print  with {} params", params.size());
            EvaluatorNodeResult result { .value = nullptr };

            if (params.size() != 1) {
                ResultMessage message { .level = error, .message = "Expected 1 param but received " + std::to_string(params.size()) };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            if (params.front()->name() != "StringNode") {
                ResultMessage message { .level = error, .message = "Expected StringNode param but received " + params.front()->name() };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            auto string_node = std::dynamic_pointer_cast<StringNode>(params.front());

            result.value = string_node;
            std::cout << string_node->get_value() << std::endl;
            return result;
        }
    };
    return func;
}

void apply_syslib(Environment& env)
{
    env.add_function(operator_add(env));
    env.add_function(operator_eq(env));
    env.add_function(operator_if(env));
    env.add_function(print(env));
}

}
