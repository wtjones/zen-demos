#include "loitar/core/node.hpp"

namespace loitar {

Node::Node() { }
Node::~Node() { }

std::ostream& operator<<(std::ostream& os, const Node& p)
{
    p.print(os);
    return os;
}

}
