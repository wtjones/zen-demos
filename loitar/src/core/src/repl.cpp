#include "loitar/core/repl.hpp"
#include "loitar/core/function.hpp"
#include "loitar/core/integer_node.hpp"
#include "loitar/core/syslib.hpp"

namespace loitar {

Repl::Repl()
{
    apply_syslib(m_env);
}
Repl::~Repl() { }

EvaluatorResult Repl::execute(std::string input)
{
    spdlog::info("repl: parsing {}", input);
    auto parsed_exp = parse(input);

    spdlog::info("repl: evaluating {}", input);
    return evaluate(m_env, parsed_exp);
}

}
