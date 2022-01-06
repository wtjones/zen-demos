#include "loitar/core/environment.hpp"

namespace loitar {

Environment::Environment() { }
Environment::~Environment() { }

void Environment::add_function(Function func)
{
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

void Environment::set_variable(std::string name, std::shared_ptr<Node> node)
{
    m_global_variables[name] = node;
}
bool Environment::has_variable(std::string name)
{
    return (m_global_variables.find(name) != m_global_variables.end());
}

std::shared_ptr<Node> Environment::get_variable(std::string name)
{
    return m_global_variables[name];
}
}