#pragma once

#include "atom_node.hpp"
#include <string>
#include <vector>

namespace loitar {
class IntegerNode : public AtomNode {
public:
    IntegerNode(std::string token, int64_t value);
    std::string name() const;
    int64_t get_value() const;
    bool operator==(const Node& node) const;
    bool operator<(const Node& node) const;
    bool operator<=(const Node& node) const;

protected:
    void print(std::ostream& os) const;

private:
    std::string m_token;
    int64_t m_value;
};
}
