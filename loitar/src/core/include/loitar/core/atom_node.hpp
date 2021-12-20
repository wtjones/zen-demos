#pragma once

#include "node.hpp"
#include <iostream>
#include <string>
#include <vector>

namespace loitar {
class AtomNode : public Node {
public:
    AtomNode(std::string token);
    std::string name() const;
    std::string get_token();

protected:
    void print(std::ostream& os) const;

private:
    std::string m_token;
};

}
