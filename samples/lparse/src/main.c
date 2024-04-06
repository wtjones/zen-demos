#include <assert.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NODE_LIST 100
#define MAX_ATOM_LENGTH 100
#define PARSE_TOKEN_MAX 255
#define PARSE_MAX_DEPTH 4
static const char* PARSE_TOKEN_OPEN_LIST = "(";
static const char* PARSE_TOKEN_CLOSE_LIST = ")";

typedef enum NodeType {
    LP_NODE_ATOM_SYMBOL,
    LP_NODE_LIST
} NodeType;

typedef struct Node {
    NodeType node_type;
    int length;
    union {
        union {
            char* symbol;
            char* string;
            int integer;
        } atom;
        struct Node* list;
    } val;
} Node;

typedef enum ParseResult {
    LP_PARSE_RESULT_OK,
    LP_PARSE_RESULT_ERROR,
    LP_PARSE_RESULT_END
} ParseResult;

typedef enum ParseTokenType {
    LP_PARSE_TOKEN_NONE,
    LP_PARSE_TOKEN_STRING,
    LP_PARSE_TOKEN_SEXP_START,
    LP_PARSE_TOKEN_SEXP_END,
    LP_PARSE_TOKEN_SEXP_SYMBOL,
} ParseTokenType;

char* strcat_alloc(char* str, const char* append)
{
    size_t old_size = str == NULL
        ? 0
        : strlen(str);

    char* result = realloc(str, old_size + strlen(append) + 1);

    result[0] = old_size == 0 ? '\0' : result[0];
    return strcat(result, append);
}

void free_expression_walk(Node* node, int depth)
{
    assert(depth <= PARSE_MAX_DEPTH);
    switch (node->node_type) {
    case LP_NODE_LIST:

        for (int i = 0; i < node->length; i++) {
            Node* node_item = &node->val.list[i];
            free_expression_walk(node_item, depth + 1);
        }
        free(node->val.list);
        break;

    case LP_NODE_ATOM_SYMBOL:
        free(node->val.atom.symbol);
        break;
    }
}

void free_expression(Node* node)
{
    free_expression_walk(node, 1);
    free(node);
}

char* repr_expression_walk(Node* node, char* work, int depth)
{
    char* result = work;
    assert(depth <= PARSE_MAX_DEPTH);

    switch (node->node_type) {
    case LP_NODE_LIST:
        result = strcat_alloc(result, PARSE_TOKEN_OPEN_LIST);

        for (int i = 0; i < node->length; i++) {

            Node* list_item = &node->val.list[i];

            result = repr_expression_walk(list_item, result, depth + 1);
            if (node->length > 1 && i != node->length - 1) {
                result = strcat_alloc(result, " ");
            }
        }
        result = strcat_alloc(result, PARSE_TOKEN_CLOSE_LIST);

        break;

    case LP_NODE_ATOM_SYMBOL:
        result = strcat_alloc(result, node->val.atom.symbol);
    }
    return result;
}

/**
 * @brief Returns a formated s-expression.
 *
 * @param node
 * @return char* Formatted expression. Must be freed.
 */
char* repr_expression(Node* node)
{
    return repr_expression_walk(node, (char*)NULL, 0);
}

ParseResult parse_token_symbol(
    const char* file_buffer,
    int* buffer_pos,
    char* token)
{
    token[0] = '\0';

    char ch[2] = "";
    ch[0] = file_buffer[(*buffer_pos)];
    while ('\0' != ch[0] && (isalnum(ch[0]) || ch[0] == '-')) {
        strcat(token, ch);
        (*buffer_pos)++;
        ch[0] = file_buffer[(*buffer_pos)];
    }

    return LP_PARSE_RESULT_OK;
}

ParseResult parse_token(
    const char* file_buffer,
    int* buffer_pos,
    char* token,
    ParseTokenType* token_type)
{
    token[0] = '\0';

    // Find first non-whitespace
    char ch = file_buffer[(*buffer_pos)];
    while ('\0' != ch && ' ' == ch) {
        (*buffer_pos)++;
        ch = file_buffer[(*buffer_pos)];
    }

    if ('\0' == ch) {
        *token_type = LP_PARSE_TOKEN_NONE;
        return LP_PARSE_RESULT_OK;
    }

    if ('(' == ch) {
        *token_type = LP_PARSE_TOKEN_SEXP_START;
        strcpy(token, "(");
        (*buffer_pos)++;
        return LP_PARSE_RESULT_OK;
    }

    if (')' == ch) {
        *token_type = LP_PARSE_TOKEN_SEXP_END;
        strcpy(token, "(");
        (*buffer_pos)++;
        return LP_PARSE_RESULT_OK;
    }

    if (isalpha(ch)) {
        *token_type = LP_PARSE_TOKEN_SEXP_SYMBOL;
        ParseResult result = parse_token_symbol(
            file_buffer, buffer_pos, token);
        return result;
    }

    fprintf(stderr, "Unhandled char: %c\n", ch);
    assert(true);
    return LP_PARSE_RESULT_ERROR;
}

Node* append_list_node(Node* node)
{
    if (node->length == 0) {
        node->length = 1;
        node->val.list = malloc(sizeof(Node) * node->length);
    } else {
        node->length++;
        node->val.list = realloc(node->val.list, sizeof(Node) * node->length);
    }
    return &node->val.list[node->length - 1];
}

ParseResult parse_list(
    const char* file_buffer,
    int* buffer_pos,
    Node* node,
    int depth)
{
    char token[PARSE_TOKEN_MAX] = "";
    ParseTokenType token_type;

    assert(depth < PARSE_MAX_DEPTH);

    node->node_type = LP_NODE_LIST;
    node->val.list = NULL;
    node->length = 0;
    while (true) {
        ParseResult result = parse_token(
            file_buffer, buffer_pos, token, &token_type);

        if (result == LP_PARSE_RESULT_ERROR) {
            return result;
        }

        switch (token_type) {
        case LP_PARSE_TOKEN_NONE:
            fprintf(stderr, "parse_list: found error\n");

            return LP_PARSE_RESULT_ERROR;
            break;

        case LP_PARSE_TOKEN_SEXP_END:
            printf("parse_list: found: )\n");

            return LP_PARSE_RESULT_OK;
            break;

        case LP_PARSE_TOKEN_SEXP_SYMBOL:
            printf("parse_list: found: symbol %s\n", token);

            Node* new_symbol_node = append_list_node(node);
            new_symbol_node->node_type = LP_NODE_ATOM_SYMBOL;
            // allocate the string?

            new_symbol_node->val.atom.symbol = malloc(strlen(token) + 1);
            strcpy(new_symbol_node->val.atom.symbol, token);
            if (*buffer_pos >= 33) {
                return LP_PARSE_RESULT_ERROR;
            }

            break;
        case LP_PARSE_TOKEN_SEXP_START:
            printf("parse_list: found: (\n");

            Node* new_list_node = append_list_node(node);
            new_list_node->node_type = LP_NODE_LIST;
            new_list_node->length = 0;

            ParseResult list_result = parse_list(
                file_buffer, buffer_pos, new_list_node, depth + 1);

            if (list_result == LP_PARSE_RESULT_ERROR) {
                return list_result;
            }

            break;

        default:
            break;
        }
    }

    return LP_PARSE_RESULT_OK;
}

ParseResult parse_raw(const char* exp, Node** node)
{
    ParseTokenType token_type;
    int buffer_pos = 0;
    char token[PARSE_TOKEN_MAX] = "";

    ParseResult token_result = parse_token(
        exp, &buffer_pos, token, &token_type);
    if (token_result == LP_PARSE_RESULT_ERROR) {
        return token_result;
    }

    printf("Called parse_token(): %s\n", token);

    if (LP_PARSE_TOKEN_SEXP_START != token_type) {
        fprintf(stderr, "Expression not found.\n");
        return LP_PARSE_RESULT_ERROR;
    }

    *node = malloc(sizeof(Node));

    if (*node == NULL) {
        fprintf(stderr, "Cannot malloc()\n");
        return LP_PARSE_RESULT_ERROR;
    }

    ParseResult exp_result = parse_list(
        exp, &buffer_pos, *node, 1);

    if (exp_result == LP_PARSE_RESULT_ERROR) {
        return exp_result;
    }
    return LP_PARSE_RESULT_OK;
}

/**
 * @brief Reads input file and returns an allocated string
 * with contents.
 *
 * @param path
 * @return char*
 */
char* read_file(const char* path)
{
    FILE* file = fopen(path, "r");

    if (!file) {
        fprintf(stderr, "Can't open file: %s\n", path);
        return NULL;
    }
    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    char* file_buffer = malloc(file_size + 1);
    if (NULL == file_buffer) {
        printf("Can't malloc()!\n");
        return NULL;
    }

    fread(file_buffer, file_size, 1, file);
    fclose(file);
    file_buffer[file_size] = '\0';
    return file_buffer;
}

int main(int argc, char* argv[])
{
    Node* node;
    if (argc == 1) {
        fprintf(stderr, "%s error: Input file required.\n", argv[0]);
        return 1;
    }

    char* file_buffer = read_file(argv[1]);

    if (file_buffer == NULL) {
        fprintf(stderr, "Error loading file %s\n", argv[1]);
        return 1;
    }
    printf("Bytes: %lu : %s", strlen(file_buffer), file_buffer);
    ParseResult result = parse_raw(file_buffer, &node);
    if (result == LP_PARSE_RESULT_ERROR) {
        fprintf(stderr, "parse_raw(): error\n");
        return 1;
    }
    free(file_buffer);
    char* pretty = repr_expression(node);

    printf("%s\n", pretty);
    free(pretty);

    free_expression(node);

    return 0;
}
