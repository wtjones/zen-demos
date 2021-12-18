#include "loitar/core/repl.hpp"

namespace loitar {

Repl::Repl() { }
Repl::~Repl() { }

EvaluatorResult Repl::execute(std::string input)
{
    spdlog::info("repl: executing {}", input);
    auto parsed_exp = parse(input);

    return evaluate(parsed_exp);
}

}
