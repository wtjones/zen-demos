#include "loitar/core/environment.hpp"
#include "loitar/core/nil_node.hpp"

namespace loitar {

Environment::Environment()
{
    m_blocks.push_back(Block());
}
Environment::~Environment() { }

void Environment::add_function(Function func)
{
    spdlog::trace("adding function {}", func.name);
    m_global_functions.insert(
        std::pair<std::string, Function>(func.name, func));
}
bool Environment::has_function(std::string name)
{
    return (m_global_functions.find(name) != m_global_functions.end());
}

Function Environment::get_function(std::string name)
{
    return m_global_functions[name];
}

void Environment::set_variable(std::string name, std::shared_ptr<Node> node, ScopeType scope)
{
    auto block = scope == ScopeType::global ? &m_blocks.front() : &m_blocks.back();
    spdlog::trace("setting {} variable {}", (scope == ScopeType::global ? "global" : "local"), name);
    block->variables[name] = node;
}

std::shared_ptr<Node> Environment::get_variable(std::string name)
{
    for (unsigned i = m_blocks.size(); i-- > 0;) {
        spdlog::debug("searching for variable {} at index {}", name, i);
        if (m_blocks[i].variables.find(name) != m_blocks[i].variables.end()) {
            spdlog::debug("searching index {} found", i);
            return m_blocks[i].variables[name];
        }
    }
    return nullptr;
}
size_t Environment::push_block()
{
    m_blocks.push_back(Block());
    spdlog::trace("Entering block level {}", m_blocks.size());
    return m_blocks.size();
}
size_t Environment::pop_block()
{
    m_blocks.pop_back();
    spdlog::trace("Entering block level {}", m_blocks.size());
    return m_blocks.size();
}
size_t Environment::block_level()
{
    return m_blocks.size();
}
}
