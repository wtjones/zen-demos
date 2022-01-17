#include "loitar/core/true_node.hpp"

namespace loitar {

TrueNode::TrueNode()
    : AtomNode("t")
{
}
std::string TrueNode::name() const
{
    return "TrueNode";
}

std::any TrueNode::value() const
{
    return std::any(true);
}

bool TrueNode::operator==(const Node& node) const
{
    return (name() == node.name());
}

void TrueNode::print(std::ostream& out) const
{
    out << m_token;
}
}
