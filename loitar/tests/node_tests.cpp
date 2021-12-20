#include "loitar/core/atom_node.hpp"
#include "loitar/core/integer_node.hpp"
#include "loitar/core/list_node.hpp"
#include "spdlog/spdlog.h"
#include <catch2/catch.hpp>
#include <map>
#include <memory>
#include <sstream>
#include <string>

using namespace loitar;

TEST_CASE("Base node should support ostream", "")
{
    const std::string expected = "atom";
    AtomNode atom_node(expected);
    Node* node = &atom_node;

    std::ostringstream ss;
    ss << *node;

    REQUIRE(ss.str() == expected);
}

TEST_CASE("AtomNode should support ostream", "")
{
    const std::string expected = "atom";
    AtomNode sut(expected);

    std::ostringstream ss;
    ss << sut;

    REQUIRE(ss.str() == expected);
}

TEST_CASE("ListNode should support ostream")
{
    std::vector<std::shared_ptr<Node>>
        inner_elements = {
            std::make_shared<AtomNode>("+"),
            std::make_shared<IntegerNode>("1", 1),
            std::make_shared<IntegerNode>("1", 1)
        };
    auto sut = std::make_shared<ListNode>(inner_elements);

    const std::string expected = "(+ 1 1)";

    std::ostringstream ss;
    ss << *sut;

    REQUIRE(ss.str() == expected);
}
