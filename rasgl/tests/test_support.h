#ifndef TEST_SUPPORT_H
#define TEST_SUPPORT_H

#include <stddef.h>
#include <string.h>

#define TEST(name) int name(void)

typedef struct TestFn {
    char* name;
    int (*fn)();
} TestFn;

TestFn* test_fn_lookup(TestFn test_fns[], size_t num_test_fns, const char* fn_name);

// Emit prototypes
#define X(name) int name(void);

#include "TestList.inc"

#undef X

#endif
