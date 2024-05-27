#include "lib_a/parse.h"
#include <stdio.h>

int main(int argc, const char** argv)
{
    int result = parse_int(5);
    printf("parse_int result: %d\n", result);

    printf("Press ENTER key to Continue\n");
    getchar();
    return 0;
}
