#include "loitar/core/array_node.hpp"
#include "loitar/core/integer_node.hpp"
#include "loitar/core/nil_node.hpp"
#include "loitar/core/string_node.hpp"
#include "spdlog/spdlog.h"
#include <catch2/catch.hpp>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

using namespace loitar;

TEST_CASE("ArrayNode should init nils", "")
{
    const auto d0 = 2, d1 = 3;
    std::vector<size_t> dimensions { d0, d1 };
    auto sut = std::make_shared<ArrayNode>(dimensions);

    REQUIRE(sut->get_elements().size() == d0 * d1);
    for (auto e : sut->get_elements()) {
        REQUIRE(e->to_string() == "nil");
    }
}

TEST_CASE("ArrayNode one dimension get element at index", "")
{
    const auto d0 = 5;
    std::vector<size_t> dimensions { d0 };
    auto sut = std::make_shared<ArrayNode>(dimensions);
    sut->set_element(std::vector<size_t> { 3 }, std::make_shared<StringNode>("hello", "hello"));

    auto result = sut->get_element(std::vector<size_t> { 3 });

    REQUIRE(result->to_string() == "hello");
}

TEST_CASE("ArrayNode two dimensions get element at index", "")
{
    const auto d0 = 3, d1 = 4;
    std::vector<size_t> dimensions { d0, d1 };
    auto sut = std::make_shared<ArrayNode>(dimensions);

    sut->set_element(std::vector<size_t> { 1, 3 }, std::make_shared<StringNode>("hello", "hello"));

    REQUIRE(sut->get_element(std::vector<size_t> { 2, 3 })->to_string() == "nil");

    REQUIRE(sut->get_element(std::vector<size_t> { 1, 3 })->to_string() == "hello");
}

TEST_CASE("ArrayNode three dimensions get element at index", "")
{
    const auto d0 = 2, d1 = 2, d2 = 2;
    std::vector<size_t> dimensions { d0, d1, d2 };
    auto sut = std::make_shared<ArrayNode>(dimensions);

    sut->set_element(std::vector<size_t> { 1, 0, 1 }, std::make_shared<StringNode>("hello", "hello"));

    REQUIRE(sut->get_element(std::vector<size_t> { 1, 1, 1 })->to_string() == "nil");

    REQUIRE(sut->get_element(std::vector<size_t> { 1, 0, 1 })->to_string() == "hello");
}
