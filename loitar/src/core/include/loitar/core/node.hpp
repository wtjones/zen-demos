#pragma once

#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <typeinfo>
#include <vector>

namespace loitar {
class Node {

public:
    Node();
    virtual ~Node();
    virtual std::string name() const = 0;
    friend std::ostream& operator<<(std::ostream& out, const Node& node);

protected:
    virtual void print(std::ostream& os) const = 0;
};
}
