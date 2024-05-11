#include "larse/core/expression.h"
#include "larse/core/parse.h"
#include "test_support.h"
#include <assert.h>
#include <stdint.h>
#include <stdio.h>

int test_parse_single()
{

    LarNode* node;

    LarParseResult result;
    result = lar_parse_single("(\"test\")", &node);
    assert(strcmp(node->list.nodes[0].atom.val_string, "test") == 0);
    printf("got string: %s\n", node->list.nodes[0].atom.val_string);
    return 0;
}

int test_parse_symbol()
{

    LarNode* node;

    LarParseResult result;
    result = lar_parse_single("(my-symbol 1 3)", &node);
    assert(strcmp(node->list.nodes[0].atom.val_symbol, "my-symbol") == 0);
    return 0;
}

int test_parse()
{
    return 0;
}

TestFn test_fns[] = {
    { "TEST_PARSE_SYMBOL", test_parse_symbol },
    { "TEST_PARSE_SINGLE", test_parse_single },
    { "TEST_PARSE", test_parse }
};

int main(int argc, const char** argv)
{

    if (argc == 1) {
        fprintf(stderr, "Test name required.\n");
        return 1;
    }

    for (int i = 1; i < argc; ++i) {
        printf("main(): argv[%d]: %s\n", i, argv[i]);
    }
    size_t num_fns = sizeof(test_fns) / sizeof(test_fns[0]);

    TestFn* test_fn = test_fn_lookup(test_fns, num_fns, argv[1]);
    if (test_fn == NULL) {
        fprintf(stderr, "Error: Test %s not found.\n", argv[1]);
        return 1;
    }
    return test_fn->fn();
}
