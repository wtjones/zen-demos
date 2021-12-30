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

int64_t IntegerNode::get_value() const
{
    return m_value;
}

bool IntegerNode::operator==(const Node& node) const
{
    if (node.name() == "TrueNode") {
        return true;
    }

    if (node.name() != this->name()) {
        return false;
    }

    return (dynamic_cast<const IntegerNode&>(node)).get_value() == m_value;
}

bool IntegerNode::operator<(const Node& node) const
{
    // can trigger std:bad_cast if types are not the same
    return m_value < (dynamic_cast<const IntegerNode&>(node)).get_value();
}

bool IntegerNode::operator<=(const Node& node) const
{
    // can trigger std:bad_cast if types are not the same
    return m_value <= (dynamic_cast<const IntegerNode&>(node)).get_value();
}

void IntegerNode::print(std::ostream& out) const
{
    out << std::to_string(m_value);
}
}
