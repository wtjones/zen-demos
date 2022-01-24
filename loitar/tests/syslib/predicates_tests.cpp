#include "loitar/core/atom_node.hpp"
#include "loitar/core/environment.hpp"
#include "loitar/core/evaluator.hpp"
#include "loitar/core/integer_node.hpp"
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

TEST_CASE("Predicate zerop returns true when zero", "")
{
    const std::string expression = "(zerop (+ -2 2))";
    const std::string expected = "t";
    Repl repl;

    auto result = repl.execute(expression);
    REQUIRE(result.messages.size() == 0);
    auto actual = result.value.back()->to_string();
    REQUIRE(expected == actual);
}

TEST_CASE("Predicate zerop returns false when non-zero", "")
{
    const std::string expression = "(zerop (+ -2 4))";
    const std::string expected = "nil";
    Repl repl;

    auto result = repl.execute(expression);
    REQUIRE(result.messages.size() == 0);
    auto actual = result.value.back()->to_string();
    REQUIRE(expected == actual);
}
