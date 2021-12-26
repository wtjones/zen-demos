#pragma once

#include "environment.hpp"
#include "evaluator.hpp"
#include "evaluator_types.hpp"
#include "parser.hpp"
#include "spdlog/spdlog.h"
#include <map>
#include <memory>
#include <string>
#include <typeinfo>
#include <vector>

namespace loitar {
class Repl {

private:
    Environment m_env;

public:
    Repl();
    virtual ~Repl();
    EvaluatorResult execute(std::string input);
};
}
