#include "loitar/core/atom_node.hpp"
#include "loitar/core/environment.hpp"
#include "loitar/core/evaluator.hpp"
#include "loitar/core/parser.hpp"
#include "loitar/core/repl.hpp"
#include "loitar/core/string_node.hpp"
#include "loitar/core/syslib/syslib.hpp"
#include "spdlog/spdlog.h"
#include <catch2/catch.hpp>
#include <map>
#include <memory>
#include <string>

using namespace loitar;

TEST_CASE("List function should return list", "")
{
    const std::string expression = "(list 1 2 3)";
    const std::string expected = "(1 2 3)";
    Repl repl;

    auto actual = repl.execute(expression).value.front()->to_string();

    REQUIRE(expected == actual);
}
