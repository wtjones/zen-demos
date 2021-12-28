#include "loitar/core/atom_node.hpp"
#include "loitar/core/integer_node.hpp"
#include "loitar/core/list_node.hpp"
#include "loitar/core/nil_node.hpp"
#include "loitar/core/true_node.hpp"
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

TEST_CASE("Node id is unique")
{
    AtomNode sut0("test");
    AtomNode sut1("test");

    REQUIRE(sut0.id() > 0);
    REQUIRE(sut1.id() > sut0.id());
}

TEST_CASE("AtomNode supports equality")
{
    AtomNode sut0("orange");
    AtomNode sut1("apple");
    AtomNode sut2("apple");

    REQUIRE(!(sut0 == sut1));
    REQUIRE(sut1 == sut2);
    REQUIRE(sut0 != sut1);
    REQUIRE(!(sut1 != sut2));
}

TEST_CASE("NilNode supports equality")
{
    NilNode sut0;
    NilNode sut1;
    TrueNode true0;
    AtomNode atom0("orange");

    REQUIRE(sut0 == sut1);
    REQUIRE(sut0 != true0);
    REQUIRE(sut0 != atom0);
}

TEST_CASE("NilNode equals emtpy list")
{
    NilNode sut0;
    ListNode empty0;
    std::vector<std::shared_ptr<Node>>
        inner_elements = {
            std::make_shared<AtomNode>("+"),
            std::make_shared<IntegerNode>("1", 1),
            std::make_shared<IntegerNode>("1", 1)
        };
    auto not_empty = ListNode(inner_elements);

    REQUIRE(sut0 == empty0);
    REQUIRE(empty0 == sut0);
    REQUIRE(sut0 != not_empty);
}
