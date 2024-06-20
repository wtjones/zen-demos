#include "larse/core/expression.h"
#include "larse/core/parse.h"
#include "larse/core/repr.h"
#include "test_support.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>

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

int test_get_string_property()
{
    const char* exp1_in = "(this :here \"val\")";
    const char* expected = "val";
    "(+ 1 2)";
    LarScript* script;
    LarParseResult result;
    result = lar_parse_script(exp1_in, &script);
    assert(result == LAR_PARSE_RESULT_OK);
    LarNode* exp1 = lar_get_first(script->expressions);
    assert(exp1 != NULL);

    LarNode* string_node;
    string_node = lar_get_string_property(exp1, ":here");
    assert(string_node != NULL);
    bool pass = strcmp(string_node->atom.val_string, expected) == 0;

    lar_free_script(&script);
    return 0;
}

TestFn test_fns[] = {
    { "TEST_PARSE_FILE", test_parse_file },
    { "TEST_PARSE_SYMBOL", test_parse_symbol },
    { "TEST_PARSE_SINGLE", test_parse_single },
    { "TEST_PARSE_SCRIPT", test_parse_script },
    { "TEST_PARSE", test_parse },
    { "TEST_REPR_EXPRESSION", test_repr_expression },
    { "TEST_REPR_SCRIPT", test_repr_script },
    { "TEST_GET_STRING_PROPERTY", test_get_string_property }
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
