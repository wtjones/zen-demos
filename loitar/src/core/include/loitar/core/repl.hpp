#pragma once

#include "evaluator.hpp"
#include "parser.hpp"
#include "spdlog/spdlog.h"
#include <map>
#include <memory>
#include <string>
#include <typeinfo>
#include <vector>

namespace loitar {
class Repl {

public:
    Repl();
    virtual ~Repl();
    EvaluatorResult execute(std::string input);
};
}
