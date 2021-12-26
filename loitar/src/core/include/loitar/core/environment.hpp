#pragma once

#include "function.hpp"
#include "spdlog/spdlog.h"
#include <map>
#include <memory>

namespace loitar {

class Environment {

private:
    std::map<std::string, Function> m_global_functions;

public:
    Environment();
    virtual ~Environment();
    void add_function(Function func);
    bool has_function(std::string name);
    Function get_function(std::string name);
};

}
