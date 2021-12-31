#pragma once

#include "function.hpp"
#include "node.hpp"
#include "spdlog/spdlog.h"
#include <map>
#include <memory>

namespace loitar {

class Environment {

private:
    std::map<std::string, Function> m_global_functions;
    std::map<std::string, std::shared_ptr<Node>> m_global_variables;

public:
    Environment();
    virtual ~Environment();
    void add_function(Function func);
    bool has_function(std::string name);
    Function get_function(std::string name);
    void set_variable(std::string name, std::shared_ptr<Node> node);
    bool has_variable(std::string name);
    std::shared_ptr<Node> get_variable(std::string name);
};

}
