
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

    auto function_node = std::dynamic_pointer_cast<AtomNode>(node->get_elements().front());

    spdlog::trace("Getting func {} from env", function_node->get_token());
    if (!env.has_function(function_node->get_token())) {
        result.messages.push_back({ .level = error, .message = "Atom " + function_node->get_token() + " is not a function" });
        spdlog::error("{}", result.messages.back().message);
        return result;
    }

    auto func = env.get_function(function_node->get_token());
    spdlog::trace("Got func {} from env", function_node->get_token());

    std::vector<std::shared_ptr<Node>> params;
    auto elements = node->get_elements();
    if (elements.size() > 1) {
        // First param is the func name atom, so omit
        std::vector<std::shared_ptr<Node>> temp_params;
        for (auto i = elements.begin() + 1; i != elements.end(); ++i) {
            temp_params.push_back(*i);
            spdlog::trace("added {} param...", (*i)->name());
        }

        if (func.eval_params) {
            auto param_result = evaluate_expression(env, temp_params, 1); // FIXME: depth
            spdlog::debug("param_result size: {}", param_result.value.size());
            for (auto p : param_result.value) {
                spdlog::trace("Adding param eval result: {}", p->name());
                params.push_back(p);
            }

            if (param_result.messages.size() > 0) {
                std::copy(param_result.messages.begin(), param_result.messages.end(), std::back_inserter(result.messages));
                return result;
            }
        } else {
            for (auto p : temp_params) {
                params.push_back(p);
            }
        }
    }

    return func.body(params);
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
        } else if (node->name() == "AtomNode") {
            auto atom_node = std::dynamic_pointer_cast<AtomNode>(node);
            auto var = env.get_variable(atom_node->get_token());
            if (var == nullptr) {
                result.messages.push_back({ .level = error, .message = "Atom " + atom_node->get_token() + " is not a variable" });
                spdlog::error("{}", result.messages.back().message);
                return result;
            }
            result.value.push_back(var);
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
