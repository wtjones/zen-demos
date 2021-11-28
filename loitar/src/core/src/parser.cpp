#include "loitar/core/parser.hpp"
#include "loitar/core/atom_node.hpp"
#include <memory>
#include <string>
namespace loitar {
std::shared_ptr<Node> parse(std::string input)
{
    return std::make_shared<AtomNode>(input);
}
}
