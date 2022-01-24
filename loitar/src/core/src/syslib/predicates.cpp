#include "loitar/core/atom_node.hpp"
#include "loitar/core/evaluator.hpp"
#include "loitar/core/integer_node.hpp"
#include "loitar/core/list_node.hpp"
#include "loitar/core/nil_node.hpp"
#include "loitar/core/true_node.hpp"
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

namespace loitar::syslib {

/**
 * If param is zero, return true
 */
Function predicate_zerop(Environment& env)
{
    Function func {
        .name = "zerop",
        .eval_params = true,
        .body = [&env](std::vector<std::shared_ptr<Node>> params) -> EvaluatorNodeResult {
            spdlog::trace("called syslib.zerop with {} params", params.size());
            EvaluatorNodeResult result { .value = nullptr };

            if (params.size() != 1) {
                ResultMessage message { .level = error, .message = "Expected 1 param but received " + std::to_string(params.size()) };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            if (params.front()->name() != "IntegerNode") {
                ResultMessage message { .level = error, .message = "Expected a number but received " + params.front()->name() };
                result.messages.push_back(message);
                spdlog::info("{}", message.message);
                return result;
            }

            // Create construct (if (eq x 0) t nil)

            spdlog::trace("zerop: creating construct if (eq x 0) t nil)");
            std::vector<std::shared_ptr<Node>> inner_elements = {
                std::make_shared<AtomNode>("="),
                params.front(),
                std::make_shared<IntegerNode>("0", 0)
            };
            std::vector<std::shared_ptr<Node>>
                elements = {
                    std::make_shared<AtomNode>("if"),
                    std::make_shared<ListNode>(inner_elements),
                    std::make_shared<TrueNode>(),
                    std::make_shared<NilNode>()
                };
            std::vector<std::shared_ptr<Node>> expressions = {
                std::make_shared<ListNode>(elements)
            };

            auto eval_result = evaluate_expression(env, expressions, 0);
            if (eval_result.messages.size() > 0) {
                std::copy(eval_result.messages.begin(), eval_result.messages.end(), std::back_inserter(result.messages));
                return result;
            }
            result.value = eval_result.value.front();
            return result;
        }
    };
    return func;
}

void apply_syslib_predicates(Environment& env)
{
    env.add_function(predicate_zerop(env));
}

}
