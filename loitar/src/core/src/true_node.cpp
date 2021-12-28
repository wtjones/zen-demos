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

void TrueNode::print(std::ostream& out) const
{
    out << m_token;
}
}
