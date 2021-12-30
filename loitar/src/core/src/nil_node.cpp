#include "loitar/core/nil_node.hpp"

namespace loitar {

NilNode::NilNode()
    : AtomNode("nil")
{
}

std::string NilNode::name() const
{
    return "NilNode";
}

bool NilNode::operator==(const Node& node) const
{
    if (node.name() == "TrueNode") {
        return false;
    }
    if (node.name() == this->name()) {
        return true;
    }
    if (node.name() == "ListNode") {
        auto list_node = dynamic_cast<const ListNode&>(node);
        return list_node.get_elements().size() == 0;
    }
    return false;
}

bool NilNode::operator!=(const Node& node) const
{
    return !(*this == node);
}

void NilNode::print(std::ostream& out) const
{
    out << "nil";
}

}
