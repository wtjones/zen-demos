
#include "loitar/core/evaluator.hpp"
#include "loitar/core/integer_node.hpp"
#include "loitar/core/list_node.hpp"
#include <memory>
#include <typeinfo>

namespace loitar {

EvaluatorNodeResult evaluate_function(Environment& env, std::shared_ptr<ListNode> node)
{
    spdlog::trace("evaluate_function");
    EvaluatorNodeResult result { .value = nullptr };

    std::vector<std::shared_ptr<Node>> eval_params;

    auto elements = node->get_elements();
    if (elements.size() > 1) {
        std::vector<std::shared_ptr<Node>> params;
        for (auto i = elements.begin() + 1; i != elements.end(); ++i) {
            params.push_back(*i);
            spdlog::trace("added {} param...", (*i)->name());
        }
        auto param_result = evaluate_expression(env, params, 1); // FIXME: depth
        spdlog::debug("param_result size: {}", param_result.value.size());
        for (auto p : param_result.value) {
            spdlog::trace("Adding param eval result: {}", p->name());
            eval_params.push_back(p);
        }
        if (param_result.messages.size() > 0) {
            std::copy(param_result.messages.begin(), param_result.messages.end(), std::back_inserter(result.messages));
            return result;
        }
    }

    auto function_node = std::dynamic_pointer_cast<AtomNode>(node->get_elements().front());

    spdlog::trace("Getting func {} from env", function_node->get_token());
    if (env.has_function(function_node->get_token())) {
        auto func = env.get_function(function_node->get_token());
        spdlog::trace("Got func {} from env", function_node->get_token());
        return func.body(eval_params);
    }

    result.messages.push_back({ .level = error, .message = "Atom " + function_node->get_token() + " is not a function" });
    spdlog::error("{}", result.messages.back().message);

    return result;
}

EvaluatorNodeResult evaluate_list_node(Environment& env, std::shared_ptr<ListNode> node)
{
    EvaluatorNodeResult result { .value = nullptr };

    if (node->get_elements().size() == 0) {
        result.value = node;
        return result;
    }

    if (node->get_elements().front()->name() != "AtomNode") {
        result.messages.push_back({ .level = error, .message = "Element 0 is not a function" });
        spdlog::error("{}", result.messages.back().message);
        return result;
    }

    return evaluate_function(env, node);
}

EvaluatorResult evaluate_expression(
    Environment& env,
    std::vector<std::shared_ptr<Node>> expression, int depth)
{
    spdlog::trace("evaluate_expression");

    EvaluatorResult result;

    for (auto node : expression) {
        spdlog::trace("Checking name of {}", node->name());

        if (node->name() == "ListNode") {
            spdlog::trace("Eval ListNode");
            auto eval_result = evaluate_list_node(env, std::dynamic_pointer_cast<ListNode>(node));
            spdlog::trace("back from evaluate_list_node");
            if (eval_result.messages.size() != 0) {
                std::copy(eval_result.messages.begin(), eval_result.messages.end(), std::back_inserter(result.messages));
            } else {
                result.value.push_back(eval_result.value);
            }
        } else {
            result.value.push_back(node);
        }
    }

    return result;
}

EvaluatorResult evaluate(Environment& env, std::vector<std::shared_ptr<Node>> expression)
{
    return evaluate_expression(env, expression, 0);
}
}
