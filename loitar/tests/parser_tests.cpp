#include "loitar/core/atom_node.hpp"
#include "loitar/core/integer_node.hpp"
#include "loitar/core/parser.hpp"
#include "loitar/core/repl.hpp"
#include "loitar/core/string_node.hpp"
#include "spdlog/spdlog.h"
#include <catch2/catch.hpp>
#include <map>
#include <memory>
#include <string>

using namespace loitar;

TEST_CASE("Atoms should parse", "")
{
    const std::string expression = "(list 1 2)";
    auto actual = parse(expression);

    auto elements = actual.front()->get_elements();

    REQUIRE(elements.size() == 3);

    auto a0 = std::dynamic_pointer_cast<AtomNode>(elements[0]);
    auto a1 = std::dynamic_pointer_cast<AtomNode>(elements[1]);
    auto a2 = std::dynamic_pointer_cast<AtomNode>(elements[2]);

    bool result = a0->get_token() == "list"
        && a1->get_token() == "1"
        && a2->get_token() == "2";

    REQUIRE(result);
}

TEST_CASE("Ints should parse", "")
{
    const std::string expression = "(+1 -2 3)";
    auto actual = parse(expression);

    auto elements = actual.front()->get_elements();

    REQUIRE(elements.size() == 3);

    auto a0 = std::dynamic_pointer_cast<IntegerNode>(elements[0]);

    auto int0 = std::any_cast<int64_t>(elements[0]->value());
    auto int1 = std::any_cast<int64_t>(elements[1]->value());
    auto int2 = std::any_cast<int64_t>(elements[2]->value());
    bool result = a0->get_token() == "+1";

    REQUIRE(result);
    REQUIRE(int0 == 1);
    REQUIRE(int1 == -2);
    REQUIRE(int2 == 3);
}

TEST_CASE("Lists should parse", "")
{
    const std::string expression = "(list 1 \"hello\" (list 8 9))";
    auto actual = parse(expression);

    auto elements = actual.front()->get_elements();
    REQUIRE(elements.size() == 4);

    auto atom0 = std::dynamic_pointer_cast<AtomNode>(elements[0]);

    REQUIRE(atom0->get_token() == "list");
    REQUIRE(
        std::dynamic_pointer_cast<AtomNode>(elements[1])->get_token() == "1");

    // nested list
    auto list1_elements = elements[3]->get_elements();
    REQUIRE(list1_elements.size() == 3);
    REQUIRE(
        std::dynamic_pointer_cast<AtomNode>(list1_elements[1])->get_token() == "8");
}

TEST_CASE("Non-list should parse", "")
{
    const std::string expression = "1 2";
    auto actual = parse(expression);

    auto int0 = std::any_cast<int64_t>(actual[0]->value());
    auto int1 = std::any_cast<int64_t>(actual[1]->value());

    REQUIRE(int0 == 1);
    REQUIRE(int1 == 2);
}

TEST_CASE("StringAtom should parse")
{
    const std::string expression = "\"Hello\"";
    auto actual = parse(expression);
    auto str0 = std::any_cast<std::string>(actual[0]->value());

    REQUIRE(str0 == "Hello");
}

TEST_CASE("StringAtom should parse with escape")
{
    const std::string expression = "\"Hello, \\\"quoted\\\"\"";
    auto actual = parse(expression);
    auto str0 = std::any_cast<std::string>(actual[0]->value());

    REQUIRE(str0 == "Hello, \"quoted\"");
}

TEST_CASE("Adjacent atom and list should parse", "")
{
    const std::string expression = "(if t(print 2)(print 4))";
    const std::string expected = "2";
    Repl repl;

    auto result = repl.execute(expression);
    REQUIRE(result.messages.size() == 0);
    auto actual = result.value.back()->to_string();
    REQUIRE(expected == actual);
}

TEST_CASE("Comments should be ignored")
{
    const std::string expression = R"(
; this is a comment
(setq a 55) ;; another
a           ;; return a
; hello
)";
    const std::string expected = "55";
    Repl repl;

    auto result = repl.execute(expression);
    REQUIRE(result.messages.size() == 0);
    auto actual = result.value.back()->to_string();
    REQUIRE(expected == actual);
}

TEST_CASE("Apostrophe wraps node in quote form", "")
{
    const std::string expression = "'(1 2 3)";
    const std::string expected = "(1 2 3)";
    Repl repl;

    auto result = repl.execute(expression);
    REQUIRE(result.messages.size() == 0);
    auto actual = result.value.back()->to_string();
    REQUIRE(expected == actual);
}
