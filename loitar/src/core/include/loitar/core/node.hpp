#pragma once

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
};
}
