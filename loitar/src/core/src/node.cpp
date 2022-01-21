#include "loitar/core/node.hpp"

namespace loitar {

Node::Node()
    : m_id(++next_id)
{
}

Node::~Node() { }

unsigned int Node::id() const
{
    return m_id;
}

std::weak_ptr<Node> Node::parent()
{
    return m_parent;
}

void Node::parent(std::weak_ptr<Node> parent_node)
{
    m_parent = parent_node;
}

std::string Node::to_string() const
{
    std::stringstream stream;
    print(stream);
    return stream.str();
}

std::ostream& operator<<(std::ostream& os, const Node& p)
{
    p.print(os);
    return os;
}
unsigned int Node::next_id = 0;
}
