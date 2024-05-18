#include "larse/core/expression.h"
#include "larse/core/parse.h"
#include "larse/core/repr.h"
// #include "test_support.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <unity.h>

void test_AverageThreeBytes_should_AverageHighValues(void)
{
    TEST_ASSERT_EQUAL_HEX8(80, 80);
}

void test_anotherthing(void)
{
    TEST_ASSERT_EQUAL_HEX8(80, 80);
}

int main(void)
{
    UNITY_BEGIN();

    RUN_TEST(test_AverageThreeBytes_should_AverageHighValues);
    RUN_TEST(test_anotherthing);

    return UNITY_END();
}

// int test_parse_single()
// {

//     LarNode* node;

//     LarParseResult result;
//     result = lar_parse_single("(\"test\")", &node);
//     assert(strcmp(node->list.nodes[0].atom.val_string, "test") == 0);
//     printf("got string: %s\n", node->list.nodes[0].atom.val_string);
//     return 0;
// }

// int test_parse_symbol()
// {

//     LarNode* node;

//     LarParseResult result;
//     result = lar_parse_single("(my-symbol 1 3)", &node);
//     assert(strcmp(node->list.nodes[0].atom.val_symbol, "my-symbol") == 0);
//     return 0;
// }

// int test_repr()
// {
//     const char* exp1_in = "(this :here\n"
//                           "(is math)\n"
//                           "(+ 1 2))";

//     const char* expected = "[List]\n"
//                            "  this [Atom: symbol]\n"
//                            "  :here [Atom: symbol]\n"
//                            "  [List]\n"
//                            "    is [Atom: symbol]\n"
//                            "    math [Atom: symbol]\n"
//                            "  [List]\n"
//                            "    + [Atom: symbol]\n"
//                            "    1 [Atom: integer]\n"
//                            "    2 [Atom: integer]";

//     LarNode* node1;
//     LarParseResult result;
//     result = lar_parse_single(exp1_in, &node1);
//     assert(result == LAR_PARSE_RESULT_OK);
//     char* actual = lar_repr_expression(node1);
//     printf("repr test:\n\nexpected:\n%s\n\nactual:\n%s\n", expected, actual);
//     bool pass = (strcmp(expected, actual) == 0);
//     free(actual);
//     return !pass;
// }

// int test_parse()
// {
//     return 0;
// }

// TestFn test_fns[] = {
//     { "TEST_PARSE_SYMBOL", test_parse_symbol },
//     { "TEST_PARSE_SINGLE", test_parse_single },
//     { "TEST_PARSE", test_parse },
//     { "TEST_REPR", test_repr }
// };

// int main(int argc, const char** argv)
// {

//     if (argc == 1) {
//         fprintf(stderr, "Test name required.\n");
//         return 1;
//     }

//     for (int i = 1; i < argc; ++i) {
//         printf("main(): argv[%d]: %s\n", i, argv[i]);
//     }
//     size_t num_fns = sizeof(test_fns) / sizeof(test_fns[0]);

//     TestFn* test_fn = test_fn_lookup(test_fns, num_fns, argv[1]);
//     if (test_fn == NULL) {
//         fprintf(stderr, "Error: Test %s not found.\n", argv[1]);
//         return 1;
//     }
//     return test_fn->fn();
// }
