#include "loitar/core/atom_node.hpp"
#include "loitar/core/integer_node.hpp"
#include "loitar/core/parser.hpp"
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

    auto elements = std::dynamic_pointer_cast<ListNode>(actual.front())->get_elements();

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

    auto elements = std::dynamic_pointer_cast<ListNode>(actual.front())->get_elements();

    REQUIRE(elements.size() == 3);

    auto a0 = std::dynamic_pointer_cast<IntegerNode>(elements[0]);
    auto a1 = std::dynamic_pointer_cast<IntegerNode>(elements[1]);
    auto a2 = std::dynamic_pointer_cast<IntegerNode>(elements[2]);
    bool result = a0->get_token() == "+1";
    REQUIRE(result);
    REQUIRE(a0->get_value() == 1);
    REQUIRE(a1->get_value() == -2);
    REQUIRE(a2->get_value() == 3);
}

TEST_CASE("Lists should parse", "")
{
    const std::string expression = "(list 1 \"hello\" (list 8 9))";
    auto actual = parse(expression);

    auto elements = std::dynamic_pointer_cast<ListNode>(actual.front())->get_elements();
    REQUIRE(elements.size() == 4);

    auto atom0 = std::dynamic_pointer_cast<AtomNode>(elements[0]);

    REQUIRE(atom0->get_token() == "list");
    REQUIRE(
        std::dynamic_pointer_cast<AtomNode>(elements[1])->get_token() == "1");

    // nested list
    auto list1_elements = std::dynamic_pointer_cast<ListNode>(elements[3])->get_elements();
    REQUIRE(list1_elements.size() == 3);
    REQUIRE(
        std::dynamic_pointer_cast<AtomNode>(list1_elements[1])->get_token() == "8");
}

TEST_CASE("Non-list should parse", "")
{
    const std::string expression = "1 2";
    auto actual = parse(expression);

    auto int0 = std::dynamic_pointer_cast<IntegerNode>(actual[0]);
    auto int1 = std::dynamic_pointer_cast<IntegerNode>(actual[1]);

    REQUIRE(int0->get_value() == 1);
    REQUIRE(int1->get_value() == 2);
}

TEST_CASE("StringAtom should parse")
{
    const std::string expression = "\"Hello\"";
    auto actual = parse(expression);
    auto str0 = std::dynamic_pointer_cast<StringNode>(actual[0]);

    REQUIRE(str0->get_value() == "Hello");
}

TEST_CASE("StringAtom should parse with escape")
{
    const std::string expression = "\"Hello, \\\"quoted\\\"\"";
    auto actual = parse(expression);
    auto str0 = std::dynamic_pointer_cast<StringNode>(actual[0]);

    REQUIRE(str0->get_value() == "Hello, \"quoted\"");
}
