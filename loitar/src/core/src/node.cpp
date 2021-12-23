#include "loitar/core/node.hpp"

namespace loitar {

Node::Node() { }
Node::~Node() { }

std::weak_ptr<Node> Node::parent() { return m_parent; }

void Node::parent(std::weak_ptr<Node> parent_node)
{
    m_parent = parent_node;
}

std::ostream& operator<<(std::ostream& os, const Node& p)
{
    p.print(os);
    return os;
}

}
