#include "loitar/core/syslib/flow_control.hpp"
#include "loitar/core/evaluator.hpp"
#include "loitar/core/list_node.hpp"
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

namespace loitar::syslib {

Function operator_cond(Environment& env)
{
    Function func {
        .name = "cond",
        .eval_params = false,
        .body = [&env](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.operator_cond with {} params", params.size());
            EvaluatorNodeResult result { .value = std::make_shared<NilNode>() };

            for (auto clause : params) {

                if (clause->name() != "ListNode") {
                    ResultMessage message { .level = error, .message = "operator_cond: Expected a ListNode but found " + clause->name() };
                    result.messages.push_back(message);
                    spdlog::info("{}", message.message);
                    return result;
                }

                if (clause->to_string() == "()") {
                    ResultMessage message { .level = error, .message = "operator_cond: Expected non-empty ListNode" };
                    result.messages.push_back(message);
                    spdlog::info("{}", message.message);
                    return result;
                }

                std::vector<std::shared_ptr<Node>> eval_param;
                eval_param.push_back(clause->get_elements().front());

                auto eval_result = evaluate_expression(env, eval_param, 0);

                spdlog::trace("evaluated cond expression...");
                if (eval_result.messages.size() > 0) {
                    spdlog::trace("evaluated true condition... bad");
                    std::copy(eval_result.messages.begin(), eval_result.messages.end(), std::back_inserter(result.messages));
                    return result;
                }

                if (*(eval_result.value.front()) == TrueNode()) {
                    spdlog::trace("operator_cond: Clause {} is true");
                    eval_param.clear();
                    eval_param.push_back(clause->get_elements().back());

                    auto eval_result = evaluate_expression(env, eval_param, 0);

                    spdlog::trace("evaluated cond true expression...");
                    if (eval_result.messages.size() > 0) {
                        spdlog::trace("evaluated true condition... bad");
                        std::copy(eval_result.messages.begin(), eval_result.messages.end(), std::back_inserter(result.messages));
                        return result;
                    }

                    result.value = eval_result.value.front();
                    return result;
                }
            }
            spdlog::trace("operator_cond result with {}", result.value->to_string());
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

Function construct_dotimes(Environment& env)
{
    Function func {
        .name = "dotimes",
        .eval_params = false,
        .body = [&env](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.construct_dotimes with {} params", params.size());
            EvaluatorNodeResult result { .value = nullptr };

            if (params.size() < 1) {
                ResultMessage message { .level = error, .message = "Expected at least 1 params but received " + std::to_string(params.size()) };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            if (params.front()->name() != "ListNode") {
                ResultMessage message { .level = error, .message = "Expected first param as ListNode but received " + params.front()->name() };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            if (params.front()->get_elements().size() != 2) {
                ResultMessage message {
                    .level = error,
                    .message = "Expected ListNode param with 2 elements but received " + std::to_string(params.front()->get_elements().size())
                };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            if (params.front()->get_elements().front()->name() != "AtomNode") {
                ResultMessage message {
                    .level = error,
                    .message = "Expected variable but received " + params.front()->to_string()
                };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            auto loop_variable = params.front()->get_elements().front();

            // evaluate n times param
            std::vector<std::shared_ptr<Node>> expressions;
            expressions.push_back(params.front()->get_elements().back());
            auto eval_result = evaluate_expression(env, expressions, 0);

            if (eval_result.messages.size() > 0) {
                std::copy(eval_result.messages.begin(), eval_result.messages.end(), std::back_inserter(result.messages));
                return result;
            }

            auto from_n = 0;
            auto to_n = std::any_cast<int64_t>(eval_result.value.front()->value());

            spdlog::trace("dotimes: Iterate variable {} {} times...", loop_variable->to_string(), to_n);

            // Params 1..n are the expressions in the body
            std::vector<std::shared_ptr<Node>> loop_expressions;
            for (auto i = params.begin() + 1; i != params.end(); ++i) {
                loop_expressions.push_back(*i);
                spdlog::trace("added {} loop statement...", (*i)->name());
            }

            auto block_level = env.push_block();
            for (auto i = from_n; i < to_n; i++) {
                env.set_variable(loop_variable->to_string(), std::make_shared<IntegerNode>("", i), ScopeType::local);
                for (auto expression : loop_expressions) {

                    expressions.clear();
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
            env.pop_block();

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
                && params[4]->to_string() == "to"
                && params[6]->to_string() == "do"
                && params[7]->name() == "ListNode") {
                spdlog::trace("Loop for x from n to n do construct..");

                std::string var_name
                    = params[1]->to_string();

                // evaluate from n times param
                std::vector<std::shared_ptr<Node>> expressions;
                expressions.push_back(params[3]);
                auto eval_result = evaluate_expression(env, expressions, 0);

                if (eval_result.messages.size() > 0) {
                    std::copy(eval_result.messages.begin(), eval_result.messages.end(), std::back_inserter(result.messages));
                    return result;
                }

                auto from_n = std::any_cast<int64_t>(eval_result.value.front()->value());

                // evaluate to n times param
                expressions.clear();
                expressions.push_back(params[5]);
                eval_result = evaluate_expression(env, expressions, 0);

                if (eval_result.messages.size() > 0) {
                    std::copy(eval_result.messages.begin(), eval_result.messages.end(), std::back_inserter(result.messages));
                    return result;
                }

                auto to_n = std::any_cast<int64_t>(eval_result.value.front()->value());

                spdlog::trace("Loop for {} from {} to {} do construct..", var_name, from_n, to_n);

                std::vector<std::shared_ptr<Node>> actions;
                for (auto i = params.begin() + 7; i != params.end(); ++i) {
                    actions.push_back(*i);
                    spdlog::trace("added {} do statement...", (*i)->name());
                }

                auto block_level = env.push_block();
                for (auto i = from_n; i <= to_n; i++) {
                    env.set_variable(var_name, std::make_shared<IntegerNode>("", i), ScopeType::local);
                    for (auto action : actions) {
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
    env.add_function(operator_cond(env));
    env.add_function(operator_if(env));
    env.add_function(construct_dotimes(env));
    env.add_function(construct_loop(env));
    env.add_function(construct_return(env));
}
}
