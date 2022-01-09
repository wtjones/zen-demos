#pragma once

#include "function.hpp"
#include "node.hpp"
#include "spdlog/spdlog.h"
#include <map>
#include <memory>
#include <vector>

namespace loitar {

enum ScopeType {
    global,
    local
};

typedef struct Block {
    std::map<std::string, Function> functions;
    std::map<std::string, std::shared_ptr<Node>> variables;
} Block;

class Environment {

private:
    std::map<std::string, Function> m_global_functions;
    std::map<std::string, std::shared_ptr<Node>> m_global_variables;
    std::vector<Block> m_blocks;

public:
    Environment();
    virtual ~Environment();
    void add_function(Function func);
    bool has_function(std::string name);
    Function get_function(std::string name);
    void set_variable(std::string name, std::shared_ptr<Node> node, ScopeType scope = ScopeType::local);
    std::shared_ptr<Node> get_variable(std::string name);
    size_t push_block();
    size_t pop_block();
    size_t block_level();
};

}
