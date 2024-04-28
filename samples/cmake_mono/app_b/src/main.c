#include "lib_a/parse.h"
#include <stdio.h>

int main(int argc, const char** argv)
{
    int result = parse_int(5);
    printf("foo result: %d\n", result);
    return 0;
}
