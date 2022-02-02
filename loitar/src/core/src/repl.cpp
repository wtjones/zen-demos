#include "loitar/core/repl.hpp"
#include "loitar/core/function.hpp"
#include "loitar/core/integer_node.hpp"
#include "loitar/core/syslib/syslib.hpp"

namespace loitar {

Repl::Repl()
{
    syslib::apply_syslib(m_env);
}
Repl::~Repl() { }

EvaluatorResult Repl::execute(std::string input)
{
    spdlog::info("repl: parsing {}", input);
    auto parsed_exp = parse(input);

    spdlog::trace("repl: parsed:");
    for (auto e : parsed_exp) {
        spdlog::trace("{}: {}", e->name(), e->to_string());
    }

    spdlog::info("repl: evaluating {}", input);
    return evaluate(m_env, parsed_exp);
}

}
