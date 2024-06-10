#include "larse/core/expression.h"
#include "larse/core/parse.h"
#include "larse/core/repr.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv[])
{
    if (argc != 3) {
        printf("Usage: %s -x \"(my-script 1 2 3)\"\n", argv[0]);
        return 1;
    }

    if (strcmp(argv[1], "-x") != 0) {
        printf("Invalid option: %s\n", argv[1]);
        return 1;
    }

    const char* script_raw = argv[2];
    LarScript* script;
    LarParseResult result = lar_parse_script(script_raw, &script);

    if (result != LAR_PARSE_RESULT_OK) {
        fprintf(stderr, "Error parsing script\n");
        return 1;
    }

    char* repr = lar_repr_script(script);
    printf("%s\n", repr);
    free(repr);
    lar_free_script(&script);

    return 0;
}
