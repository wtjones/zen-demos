#include "larse/core/expression.h"
#include "larse/core/parse.h"
#include "larse/core/repr.h"
#include "test_support.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>

int test_parse_comment()
{
    // Arrange
    const char* file_path = TEST_DATA_DIR "/comment1.lsp";
    const size_t expected_expressions = 2;
    const size_t expected_calcs = 2;
    LarScript* script;
    LarParseResult result;

    // Act
    result = lar_parse_file(file_path, &script);

    // Assert
    assert(result == LAR_PARSE_RESULT_OK);
    bool pass = script->expressions->list.length == expected_expressions;

    LarNode* last_exp = lar_get_list_by_symbol(script->expressions, "some-calc");

    const size_t actual_length = last_exp->list.length;
    pass = pass && actual_length - 1 == expected_calcs;

    lar_free_script(&script);
    return !pass;
}

int test_parse_file()
{
    const char* file_path = TEST_DATA_DIR "/test1.lsp";
    const size_t expected_expressions = 2;
    LarScript* script;
    LarParseResult result;
    result = lar_parse_file(file_path, &script);
    assert(result == LAR_PARSE_RESULT_OK);
    bool pass = script->expressions->list.length == expected_expressions;

    lar_free_script(&script);
    return !pass;
}

int test_parse_single()
{

    LarNode* node;

    LarParseResult result;
    result = lar_parse_single("(\"test\")", &node);
    assert(strcmp(node->list.nodes[0].atom.val_string, "test") == 0);
    printf("got string: %s\n", node->list.nodes[0].atom.val_string);
    lar_free_expression(&node);
    return 0;
}

int test_parse_symbol()
{
    LarNode* node;

    LarParseResult result;
    result = lar_parse_single("(my-symbol 1 3)", &node);
    assert(strcmp(node->list.nodes[0].atom.val_symbol, "my-symbol") == 0);
    lar_free_expression(&node);

    return 0;
}

int test_parse_script()
{
    const char* script_in = "(expr1 (howdy))\n"
                            "(expr2 :hi)(expr3 1 2 3)";
    const size_t expected_expressions = 3;
    LarScript* script;
    LarParseResult result;
    int buffer_pos;

    result = lar_parse_script(script_in, &script);
    assert(result == LAR_PARSE_RESULT_OK);
    const size_t actual_expressions = script->expressions->list.length;
    bool pass = expected_expressions == actual_expressions;

    LarNode* last_exp = &script->expressions->list.nodes[2];
    bool found_atom = strcmp(last_exp->list.nodes[0].atom.val_symbol, "expr3") == 0;
    pass = pass && found_atom;
    lar_free_script(&script);
    return !pass;
}

int test_repr_expression()
{
    const char* exp1_in = "(this :here\n"
                          "(is math)\n"
                          "(+ 1 2))";

    const char* expected = "[List]\n"
                           "  this [Atom: symbol]\n"
                           "  :here [Atom: symbol]\n"
                           "  [List]\n"
                           "    is [Atom: symbol]\n"
                           "    math [Atom: symbol]\n"
                           "  [List]\n"
                           "    + [Atom: symbol]\n"
                           "    1 [Atom: integer]\n"
                           "    2 [Atom: integer]";

    LarNode* node1;
    LarParseResult result;
    result = lar_parse_single(exp1_in, &node1);
    assert(result == LAR_PARSE_RESULT_OK);
    char* actual = lar_repr_expression(node1);
    printf("repr test:\n\nexpected:\n%s\n\nactual:\n%s\n", expected, actual);
    bool pass = (strcmp(expected, actual) == 0);
    lar_free_expression(&node1);
    free(actual);
    return !pass;
}

int test_repr_script()
{
    const char* exp1_in = "(this :here (is math))\n"
                          "(+ 1 2)";

    const char* expected = "[Script]\n"
                           "  [List]\n"
                           "    this [Atom: symbol]\n"
                           "    :here [Atom: symbol]\n"
                           "    [List]\n"
                           "      is [Atom: symbol]\n"
                           "      math [Atom: symbol]\n"
                           "  [List]\n"
                           "    + [Atom: symbol]\n"
                           "    1 [Atom: integer]\n"
                           "    2 [Atom: integer]";

    LarScript* script;
    LarParseResult result;
    result = lar_parse_script(exp1_in, &script);
    assert(result == LAR_PARSE_RESULT_OK);
    char* actual = lar_repr_script(script);
    printf("repr test:\n\nexpected:\n%s\n\nactual:\n%s\n", expected, actual);
    bool pass = (strcmp(expected, actual) == 0);
    lar_free_script(&script);
    free(actual);
    return !pass;
}

int test_parse()
{
    return 0;
}

int test_get_property()
{
    // arrange
    const char* exp1_in = "(this :my_string \"val\" :my_int 1 :my_nothing)";
    const char* expected_string = "val";
    const int expected_int = 1;
    const char* expected_nothing = NULL;

    LarScript* script;
    LarParseResult result;
    result = lar_parse_script(exp1_in, &script);
    assert(result == LAR_PARSE_RESULT_OK);
    LarNode* exp1 = lar_get_first(script->expressions);
    assert(exp1 != NULL);

    // act
    LarNode* string_node = lar_get_property_by_type(
        exp1, ":my_string", LAR_NODE_ATOM_STRING);
    LarNode* int_node = lar_get_property_by_type(
        exp1, ":my_int", LAR_NODE_ATOM_INTEGER);
    LarNode* nothing_node = lar_get_property_by_type(
        exp1, ":my_nothing", LAR_NODE_ATOM_SYMBOL);

    // assert
    assert(string_node != NULL);
    bool pass = true;
    pass = string_node != NULL
        && strcmp(string_node->atom.val_string, expected_string) == 0;
    if (!pass) {
        printf("expected: %s\n", expected_string);
        printf("actual: %s\n", string_node->atom.val_string);
    }
    pass = pass && int_node != NULL
        && int_node->atom.val_integer == expected_int;
    if (!pass) {
        printf("expected: %d\n", expected_int);
        printf("actual: %d\n", int_node->atom.val_integer);
    }
    pass = pass && nothing_node == NULL;
    if (!pass) {
        printf("expected: NULL\n");
    }
    lar_free_script(&script);
    return !pass;
}

int test_get_list()
{
    // arrange
    const char* exp1_in = "(this :foo test (models 1 2 3 4 5))";

    LarScript* script;
    LarParseResult result;
    result = lar_parse_script(exp1_in, &script);
    assert(result == LAR_PARSE_RESULT_OK);
    LarNode* exp1 = lar_get_first(script->expressions);
    assert(exp1 != NULL);

    // act
    LarNode* list_node = lar_get_list_by_symbol(exp1, "models");

    // assert
    assert(list_node != NULL);
    bool pass = list_node->node_type == LAR_NODE_LIST;
    pass = pass && list_node->list.length == 6;

    lar_free_script(&script);
    return !pass;
}

TestFn test_fns[] = {
    { "TEST_PARSE_COMMENT", test_parse_comment },
    { "TEST_PARSE_FILE", test_parse_file },
    { "TEST_PARSE_SYMBOL", test_parse_symbol },
    { "TEST_PARSE_SINGLE", test_parse_single },
    { "TEST_PARSE_SCRIPT", test_parse_script },
    { "TEST_PARSE", test_parse },
    { "TEST_REPR_EXPRESSION", test_repr_expression },
    { "TEST_REPR_SCRIPT", test_repr_script },
    { "TEST_GET_PROPERTY", test_get_property },
    { "TEST_GET_LIST", test_get_list }
};

int main(int argc, const char** argv)
{
    if (argc == 1) {
        fprintf(stderr, "Test name required.\n");
        return 1;
    }

    printf("Test data directory: %s\n", TEST_DATA_DIR);
    size_t num_fns = sizeof(test_fns) / sizeof(test_fns[0]);

    TestFn* test_fn = test_fn_lookup(test_fns, num_fns, argv[1]);
    if (test_fn == NULL) {
        fprintf(stderr, "Error: Test %s not found.\n", argv[1]);
        return 1;
    }
    return test_fn->fn();
}
