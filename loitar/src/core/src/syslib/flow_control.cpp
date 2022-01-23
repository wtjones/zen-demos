#include "loitar/core/syslib/flow_control.hpp"
#include "loitar/core/evaluator.hpp"
#include "loitar/core/list_node.hpp"
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

namespace loitar::syslib {

Function operator_if(Environment& env)
{
    Function func {
        .name = "if",
        .eval_params = false,
        .body = [&env](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.operator_if with {} params", params.size());
            EvaluatorNodeResult result { .value = nullptr };

            if (params.size() < 2 || params.size() > 3) {
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

Function construct_loop(Environment& env)
{
    Function func {
        .name = "loop",
        .eval_params = false,
        .body = [&env](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.construct_loop with {} params", params.size());
            EvaluatorNodeResult result { .value = nullptr };

            if (params.size() == 0 || params.front()->name() == "ListNode") {
                auto block_level = env.push_block();
                while (true) {
                    for (auto expression : params) {
                        std::vector<std::shared_ptr<Node>> expressions;
                        expressions.push_back(expression);
                        auto eval_result = evaluate_expression(env, expressions, 0);

                        if (eval_result.messages.size() > 0) {
                            std::copy(eval_result.messages.begin(), eval_result.messages.end(), std::back_inserter(result.messages));
                            return result;
                        }
                        result.value = eval_result.value.front();

                        if (block_level != env.block_level()) {
                            spdlog::trace("Block level changed during loop. Returning...");
                            return result;
                        }
                    }
                }
            } else if (params.size() >= 8
                && params.front()->to_string() == "for"
                && params[1]->name() == "AtomNode"
                && params[2]->to_string() == "from"
                && params[3]->name() == "IntegerNode"
                && params[4]->to_string() == "to"
                && params[5]->name() == "IntegerNode"
                && params[6]->to_string() == "do"
                && params[7]->name() == "ListNode") {
                spdlog::trace("Loop for x from n to n do construct..");

                std::string var_name
                    = params[1]->to_string();

                auto from_n = std::any_cast<int64_t>(params[3]->value());
                auto to_n = std::any_cast<int64_t>(params[5]->value());

                spdlog::trace("Loop for {} from {} to {} do construct..", var_name, from_n, to_n);
                auto action = params[7];

                auto block_level = env.push_block();
                for (auto i = from_n; i <= to_n; i++) {
                    env.set_variable(var_name, std::make_shared<IntegerNode>("", i), ScopeType::local);
                    spdlog::trace("Loop for construct: evaluating do expression...");
                    std::vector<std::shared_ptr<Node>> expressions;
                    expressions.push_back(action);
                    auto eval_result = evaluate_expression(env, expressions, 0);

                    if (eval_result.messages.size() > 0) {
                        std::copy(eval_result.messages.begin(), eval_result.messages.end(), std::back_inserter(result.messages));
                        return result;
                    }

                    result.value = eval_result.value.front();

                    if (block_level != env.block_level()) {
                        spdlog::trace("Block level changed during loop. Returning...");
                        return result;
                    }
                }
                env.pop_block();
            } else {
                ResultMessage message { .level = error, .message = "Unsupported loop for construct." };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }
            return result;
        }
    };

    return func;
}

Function construct_return(Environment& env)
{
    Function func {
        .name = "return",
        .eval_params = false,
        .body = [&env](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.construct_return with {} params", params.size());
            EvaluatorNodeResult result { .value = nullptr };

            if (params.size() > 1) {
                ResultMessage message { .level = error, .message = "Expected 0 or 1 params but received " + std::to_string(params.size()) };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            if (params.size() == 1) {
                std::vector<std::shared_ptr<Node>> expressions;
                expressions.push_back(params.front());
                auto eval_result = evaluate_expression(env, expressions, 0);
                if (eval_result.messages.size() > 0) {
                    std::copy(eval_result.messages.begin(), eval_result.messages.end(), std::back_inserter(result.messages));
                    return result;
                }
                result.value = eval_result.value.front();
            }

            env.pop_block();
            return result;
        }
    };

    return func;
}

void apply_syslib_flow_control(Environment& env)
{
    env.add_function(operator_if(env));
    env.add_function(construct_loop(env));
    env.add_function(construct_return(env));
}
}
