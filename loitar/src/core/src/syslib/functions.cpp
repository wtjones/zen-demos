#include "loitar/core/syslib/functions.hpp"
#include "loitar/core/atom_node.hpp"
#include "loitar/core/evaluator.hpp"
#include "loitar/core/list_node.hpp"
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

namespace loitar::syslib {

/**
 * Add a user-defined function into scope
 * Example: (defun sum (a b) (+ a b))
 */
Function defun(Environment& env)
{
    Function func {
        .name = "defun",
        .eval_params = false,
        .body = [&env](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.defun with {} params", params.size());
            EvaluatorNodeResult result { .value = nullptr };

            // At minimum the name (atom) and params (list) are required
            if (params.size() < 2) {
                ResultMessage message { .level = error, .message = "Expected at least 2 param but received " + std::to_string(params.size()) };
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

            if (params[1]->name() != "ListNode") {
                ResultMessage message { .level = error,
                    .message = "Expected ListNode param but received " + params[1]->name() + ": " + params[1]->to_string() };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            // The params should consist of an empty list or a list of atoms defining the param names
            auto param_list = std::dynamic_pointer_cast<ListNode>(params[1]);
            std::vector<std::shared_ptr<AtomNode>> func_params;

            for (auto element : param_list->get_elements()) {
                if (element->name() != "AtomNode") {
                    ResultMessage message { .level = error, .message = "Expected AtomNode for function param definition but received " + params.back()->name() };
                    result.messages.push_back(message);
                    spdlog::info("{}", message.message);
                    return result;
                }
                func_params.push_back(std::dynamic_pointer_cast<AtomNode>(element));
            }

            // Params 3..n are the expressions in the body
            std::vector<std::shared_ptr<Node>> func_expressions;
            for (auto i = params.begin() + 2; i != params.end(); ++i) {
                func_expressions.push_back(*i);
                spdlog::trace("added {} func statement...", (*i)->name());
            }

            auto func_name = std::dynamic_pointer_cast<AtomNode>(params.front())->get_token();

            Function func {
                .name = func_name,
                .eval_params = true,
                .body = [&env, func_params, func_expressions](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
                    EvaluatorNodeResult result { .value = std::make_shared<NilNode>() };

                    if (params.size() != func_params.size()) {
                        ResultMessage message {
                            .level = error,
                            .message = "Function defines " + std::to_string(func_params.size()) + " params but received " + std::to_string(params.size())
                        };
                        result.messages.push_back(message);
                        spdlog::info("{}", message.message);
                        return result;
                    }

                    auto block_level = env.push_block();
                    for (auto i = 0; i < func_params.size(); i++) {
                        spdlog::trace("Setting func param {} to local scope", func_params[i]->get_token());
                        env.set_variable(func_params[i]->get_token(), params[i], ScopeType::local);
                    }

                    for (auto expression : func_expressions) {
                        std::vector<std::shared_ptr<Node>> expressions;
                        expressions.push_back(expression);
                        auto eval_result = evaluate_expression(env, expressions, 0);

                        if (eval_result.messages.size() > 0) {
                            std::copy(eval_result.messages.begin(), eval_result.messages.end(), std::back_inserter(result.messages));
                            return result;
                        }
                        result.value = eval_result.value.front();

                        if (block_level != env.block_level()) {
                            spdlog::trace("Block level changed during function. Returning...");
                            return result;
                        }
                    }
                    env.pop_block();
                    return result;
                }
            };

            env.add_function(func);
            result.value = params.front();
            return result;
        }
    };
    return func;
}

void apply_syslib_functions(Environment& env)
{
    env.add_function(defun(env));
}

}
