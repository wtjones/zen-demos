#include "loitar/core/integer_node.hpp"

namespace loitar {

IntegerNode::IntegerNode(std::string token, int64_t value)
    : AtomNode(token)
{
    m_value = value;
}
std::string IntegerNode::name() const
{
    return "IntegerNode";
}

std::any IntegerNode::value() const
{
    return std::any(m_value);
}

int64_t IntegerNode::value2() const
{
    return m_value;
}

bool IntegerNode::operator==(const Node& node) const
{
    if (node.name() != this->name()) {
        return false;
    }

    return m_value == std::any_cast<int64_t>(node.value());
}

bool IntegerNode::operator<(const Node& node) const
{
    // can trigger std:bad_cast if types are not the same
    return m_value < std::any_cast<int64_t>(node.value());
}

bool IntegerNode::operator<=(const Node& node) const
{
    // can trigger std:bad_cast if types are not the same
    return m_value <= std::any_cast<int64_t>(node.value());
}

void IntegerNode::print(std::ostream& out) const
{
    out << std::to_string(m_value);
}
}
