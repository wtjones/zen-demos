
#include "loitar/core/evaluator.hpp"
#include "loitar/core/integer_node.hpp"
#include "loitar/core/list_node.hpp"
#include <memory>
#include <typeinfo>

namespace loitar {

EvaluatorNodeResult operator_add(std::vector<std::shared_ptr<Node>> params)
{
    spdlog::trace("operator_add with {} params", params.size());
    EvaluatorNodeResult result { .value = nullptr };
    auto sum = 0;
    for (auto node : params) {
        if (node->name() == "IntegerNode") {
            auto int_node = std::dynamic_pointer_cast<IntegerNode>(node);
            spdlog::trace("Summing {}", int_node->get_value());
            sum += int_node->get_value();
        } else if (node->name() == "ListNode") {
            auto list_node = std::dynamic_pointer_cast<ListNode>(node);
            auto list_result = evaluate_list_node(list_node);

            if (list_result.messages.size() > 0) {
                std::copy(list_result.messages.begin(), list_result.messages.end(), std::back_inserter(result.messages));
                return result;
            }
            if (list_result.value->name() != "IntegerNode") {
                ResultMessage message { .level = error, .message = "Expected ListNode to evaluate to IntegerNode but received " + list_result.value->name() };
                result.messages.push_back(message);
                spdlog::error("{}", message.message);
                return result;
            } else {
                auto int_node = std::dynamic_pointer_cast<IntegerNode>(list_result.value);
                spdlog::trace("Summing {}", int_node->get_value());
                sum += int_node->get_value();
            }
        }
    }
    result.value = std::make_shared<IntegerNode>("", sum);
    spdlog::trace("operator_add result with {}", sum);
    return result;
}

EvaluatorNodeResult evaluate_function(std::shared_ptr<ListNode> node)
{
    spdlog::trace("evaluate_function");
    EvaluatorNodeResult result { .value = nullptr };
    std::vector<std::shared_ptr<Node>> params;

    auto elements = node->get_elements();
    if (elements.size() > 1) {
        for (auto i = elements.begin() + 1; i != elements.end(); ++i) {
            params.push_back(*i);
            spdlog::trace("add param...");
        }
    }
    auto function_node = std::dynamic_pointer_cast<AtomNode>(node->get_elements().front());
    if (function_node->get_token() == "+") {
        spdlog::trace("Add operation...");
        return operator_add(params);
    }
    result.messages.push_back({ .level = error, .message = "Atom " + function_node->get_token() + " is not a function" });
    spdlog::error("{}", result.messages.back().message);

    return result;
}

EvaluatorNodeResult evaluate_list_node(std::shared_ptr<ListNode> node)
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

    return evaluate_function(node);
}

EvaluatorNodeResult evaluate_integer_node(std::shared_ptr<IntegerNode> node)
{
    EvaluatorNodeResult result { .value = node };
    return result;
}

EvaluatorResult evaluate_expression(
    std::vector<std::shared_ptr<Node>> expression, int depth)
{
    spdlog::trace("evaluate_expression");

    EvaluatorResult result;

    for (auto node : expression) {
        spdlog::trace("Checking name of {}", node->name());

        if (node->name() == "IntegerNode") {
            auto int_node = std::dynamic_pointer_cast<IntegerNode>(node);
            spdlog::trace("Eval IntegerNode");
            auto eval_result = evaluate_integer_node(int_node);
            result.value.push_back(eval_result.value);
        } else if (node->name() == "ListNode") {
            spdlog::trace("Eval ListNode");
            auto eval_result = evaluate_list_node(std::dynamic_pointer_cast<ListNode>(node));
            spdlog::trace("back from evaluate_list_node");
            if (eval_result.messages.size() != 0) {
                std::copy(eval_result.messages.begin(), eval_result.messages.end(), std::back_inserter(result.messages));
            } else {
                result.value.push_back(eval_result.value);
            }
        }
    }

    return result;
}

EvaluatorResult evaluate(std::vector<std::shared_ptr<Node>> expression)
{
    return evaluate_expression(expression, 0);
}
}
