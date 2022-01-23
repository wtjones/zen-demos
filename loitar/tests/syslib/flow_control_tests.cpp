#include "loitar/core/repl.hpp"
#include "spdlog/spdlog.h"
#include <catch2/catch.hpp>
#include <map>
#include <memory>
#include <string>

using namespace loitar;

TEST_CASE("Loop for from construct iterates", "")
{
    const std::string expression = R"(
(setq *total* 0)
(loop for a from 1 to 3
    do (setq *total* (+ a *total*)))
*total*
)";
    const std::string expected = "6";
    Repl repl;
    auto actual = repl.execute(expression).value.back()->to_string();

    REQUIRE(expected == actual);
}

TEST_CASE("Loop for from construct can return", "")
{
    const std::string expression = R"(
(loop for a from 10 to 20
    do (return a))
)";
    const std::string expected = "10";
    Repl repl;
    auto actual = repl.execute(expression).value.back()->to_string();

    REQUIRE(expected == actual);
}
