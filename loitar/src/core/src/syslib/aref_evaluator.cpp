
#include "loitar/core/syslib/aref_evaluator.hpp"

namespace loitar::syslib {

ArefEvaluator::ArefEvaluator() {};
ArefEvaluator::~ArefEvaluator() {};

EvaluatorNodeResult ArefEvaluator::evaluate(std::vector<std::shared_ptr<Node>> params)
{
    m_params = params;
    params.front()->accept(*this);

    return m_result;
}

void ArefEvaluator::visit(IntegerNode& node)
{
    spdlog::trace("visit Integer");

    m_indexes.push_back(node.value2());
}

void ArefEvaluator::visit(StringNode& node)
{
    spdlog::trace("visit StringNode");

    for (auto i = m_params.begin() + 1; i != m_params.end(); ++i) {
        (*i)->accept(*this);
    }

    if (m_indexes.size() < 1) {
        ResultMessage message { .level = error, .message = "Expected positional index for string." };
        m_result.messages.push_back(message);
        spdlog::info("{}", message.message);
        return;
    }

    std::string str_at(1, node.to_string().at(m_indexes.front()));
    m_result.value = std::make_shared<StringNode>("", str_at);
}

void ArefEvaluator::visit(ArrayNode& node)
{
    // collect index params
    for (auto i = m_params.begin() + 1; i != m_params.end(); ++i) {
        (*i)->accept(*this);
    }
    if (m_indexes.size() != node.dimensions().size()) {
        ResultMessage message { .level = error, .message = "Expected positional indexes to match array dimensions." };
        m_result.messages.push_back(message);
        spdlog::info("{}", message.message);
        return;
    }
    m_result.value = node.get_element(m_indexes);
}

}
