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

}
