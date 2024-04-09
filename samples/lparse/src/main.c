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
    LP_NODE_ATOM_STRING,
    LP_NODE_ATOM_BOOLEAN,
    LP_NODE_LIST
} NodeType;

typedef struct Node {
    NodeType node_type;
    union {
        union {
            char* symbol;
            char* string;
            int integer;
            bool val_bool;
        } atom;
        struct {
            size_t length;
            struct Node* nodes;
        } list;
    };
} Node;

typedef enum ParseResult {
    LP_PARSE_RESULT_OK,
    LP_PARSE_RESULT_ERROR,
    LP_PARSE_RESULT_END,
    LP_PARSE_RESULT_PASS
} ParseResult;

typedef enum ParseExpressionType {
    LP_PARSE_EXP_NONE,
    LP_PARSE_EXP_ATOM,
    LP_PARSE_EXP_LIST_START,
    LP_PARSE_EXP_LIST_END,
} ParseExpressionType;

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

        for (size_t i = 0; i < node->list.length; i++) {
            Node* node_item = &node->list.nodes[i];
            free_expression_walk(node_item, depth + 1);
        }
        free(node->list.nodes);
        break;

    case LP_NODE_ATOM_SYMBOL:
        free(node->atom.symbol);
        break;

    case LP_NODE_ATOM_STRING:
        free(node->atom.string);
        break;

    default:
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

        for (size_t i = 0; i < node->list.length; i++) {

            Node* list_item = &node->list.nodes[i];

            result = repr_expression_walk(list_item, result, depth + 1);
            if (node->list.length > 1 && i != node->list.length - 1) {
                result = strcat_alloc(result, " ");
            }
        }
        result = strcat_alloc(result, PARSE_TOKEN_CLOSE_LIST);

        break;

    case LP_NODE_ATOM_SYMBOL:
        result = strcat_alloc(result, node->atom.symbol);
        break;

    case LP_NODE_ATOM_STRING:
        result = strcat_alloc(result, "\"");
        result = strcat_alloc(result, node->atom.string);
        result = strcat_alloc(result, "\"");
        break;

    case LP_NODE_ATOM_BOOLEAN:
        result = strcat_alloc(result, node->atom.val_bool ? "true" : "false");
        break;
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

/**
 * @brief Is this a valid char to trail an atom?
 *
 * @param ch
 * @return true
 * @return false
 */
bool is_atom_end_char(char ch)
{
    char end_chars[] = " )(\n";
    return strchr(end_chars, ch) != NULL;
}

/**
 * @brief Rries to parse a boolean out of given position.
 * If a valid boolean [yes|no], node is populated and result is OK
 * If not a boolean, result is PASS.
 *
 * @param file_buffer
 * @param buffer_pos
 * @param node
 * @return ParseResult
 */
ParseResult parse_token_atom_boolean(const char* file_buffer,
    int* buffer_pos,
    Node* node)
{
    char token[PARSE_TOKEN_MAX] = "";
    char ch[2] = "";
    int pos = *buffer_pos;

    ch[0] = file_buffer[pos];
    assert(ch[0] != '\0');

    while (!is_atom_end_char(ch[0])) {
        strcat(token, ch);
        pos++;
        ch[0] = file_buffer[pos];
    }

    bool is_true = strcmp(token, "true") == 0;
    bool is_false = strcmp(token, "false") == 0;

    if (!is_true && !is_false) {
        return LP_PARSE_RESULT_PASS;
    }

    printf("Boolean found, adding to node: %s\n", is_true ? "true" : "false");
    node->node_type = LP_NODE_ATOM_BOOLEAN;
    node->atom.val_bool = is_true;
    *buffer_pos = pos;

    return LP_PARSE_RESULT_OK;
}

/**
 * @brief Tries to parse a string out of given position.
 * If a valid string, node is populated and result is OK
 * If a string but not valid, result is ERROR.
 * If not a string, result is PASS.
 * A valid string starts with a double quote.
 *
 * @param file_buffer
 * @param buffer_pos
 * @param node
 * @return ParseResult
 */
ParseResult parse_token_atom_string(const char* file_buffer,
    int* buffer_pos,
    Node* node)
{
    char token[PARSE_TOKEN_MAX] = "";
    char ch[2] = "";

    ch[0] = file_buffer[(*buffer_pos)];
    assert(ch[0] != '\0');

    if (ch[0] != '"') {
        return LP_PARSE_RESULT_PASS;
    }

    (*buffer_pos)++;
    ch[0] = file_buffer[(*buffer_pos)];
    while (true) {
        if (ch[0] == '\0') {
            return LP_PARSE_RESULT_ERROR;
        }

        if (ch[0] == '"') {
            (*buffer_pos)++;
            break;
        }

        if (ch[0] == '\\') {
            (*buffer_pos)++;
            ch[0] = file_buffer[(*buffer_pos)];
            if (ch[0] != '"' && ch[0] != '\\') {
                fprintf(stderr, "Backslash must be followed by \" or \\. Instead found: %s\n", ch);
                return LP_PARSE_RESULT_ERROR;
            }
        }
        strcat(token, ch);
        (*buffer_pos)++;
        ch[0] = file_buffer[(*buffer_pos)];
    }

    printf("String found, adding to node: \"%s\"\n", token);
    node->node_type = LP_NODE_ATOM_STRING;
    node->atom.string = malloc(strlen(token) + 1);
    strcpy(node->atom.string, token);
    return LP_PARSE_RESULT_OK;
}

bool is_valid_symbol_start(char ch)
{
    char special_chars[] = "+-*/_!?$%%&=<>@^~:";
    return (!isdigit(ch) && (isalpha(ch) || strchr(special_chars, ch) != NULL));
}

bool is_valid_symbol(char ch)
{
    char special_chars[] = "+-*/_!?$%%&=<>@^~:";
    return (isalnum(ch) || strchr(special_chars, ch) != NULL);
}

ParseResult parse_token_atom_symbol(
    const char* file_buffer,
    int* buffer_pos,
    Node* node)
{
    char token[PARSE_TOKEN_MAX] = "";
    char ch[2] = "";
    int pos = *buffer_pos;

    ch[0] = file_buffer[pos];

    if (!is_valid_symbol_start(ch[0])) {
        return LP_PARSE_RESULT_PASS;
    }

    while ('\0' != ch[0] && !is_atom_end_char(ch[0])) {
        if (!is_valid_symbol(ch[0])) {
            fprintf(stderr, "parse_token_atom_symbol: Invalid char: %c\n", ch[0]);
            return LP_PARSE_RESULT_ERROR;
        }
        strcat(token, ch);
        pos++;
        ch[0] = file_buffer[pos];
    }

    printf("Symbol found, adding to node: %s\n", token);
    *buffer_pos = pos;
    node->node_type = LP_NODE_ATOM_SYMBOL;
    node->atom.symbol = malloc(strlen(token) + 1);
    strcpy(node->atom.symbol, token);

    return LP_PARSE_RESULT_OK;
}

ParseResult parse_token_atom(
    const char* file_buffer,
    int* buffer_pos,
    Node* node)
{
    ParseResult result;

    result = parse_token_atom_string(file_buffer, buffer_pos, node);
    if (result != LP_PARSE_RESULT_PASS) {
        printf("parse_token_atom: found atom string\n");
        return result;
    }

    result = parse_token_atom_boolean(file_buffer, buffer_pos, node);
    if (result != LP_PARSE_RESULT_PASS) {
        printf("parse_token_atom: found atom boolean\n");
        return result;
    }

    result = parse_token_atom_symbol(file_buffer, buffer_pos, node);
    if (result != LP_PARSE_RESULT_PASS) {
        printf("parse_token_atom: found atom symbol\n");
        return result;
    }

    fprintf(stderr, "parse_token_atom: atom not identifed at pos %d\n", *buffer_pos);
    return LP_PARSE_RESULT_ERROR;
}

/**
 * @brief Seek to next token and identify next top-level expression type
 *
 * @param file_buffer
 * @param buffer_pos
 * @param token_type
 * @return ParseResult
 */
ParseResult seek_expression(
    const char* file_buffer,
    int* buffer_pos,
    ParseExpressionType* exp_type)
{
    // Find first non-whitespace
    char ch = file_buffer[(*buffer_pos)];
    while ('\0' != ch && ' ' == ch) {
        (*buffer_pos)++;
        ch = file_buffer[(*buffer_pos)];
    }

    if ('\0' == ch) {
        *exp_type = LP_PARSE_EXP_NONE;
        return LP_PARSE_RESULT_OK;
    }

    if ('(' == ch) {
        *exp_type = LP_PARSE_EXP_LIST_START;

        return LP_PARSE_RESULT_OK;
    }

    if (')' == ch) {
        *exp_type = LP_PARSE_EXP_LIST_END;
        return LP_PARSE_RESULT_OK;
    }

    *exp_type = LP_PARSE_EXP_ATOM;
    return LP_PARSE_RESULT_OK;
}

Node* append_list_node(Node* node)
{
    if (node->list.length == 0) {
        node->list.length = 1;
        node->list.nodes = malloc(sizeof(Node) * node->list.length);
    } else {
        node->list.length++;
        node->list.nodes = realloc(node->list.nodes, sizeof(Node) * node->list.length);
    }
    return &node->list.nodes[node->list.length - 1];
}

ParseResult parse_list(
    const char* file_buffer,
    int* buffer_pos,
    Node* node,
    int depth)
{
    ParseExpressionType exp_type;

    assert(depth < PARSE_MAX_DEPTH);

    node->node_type = LP_NODE_LIST;
    node->list.nodes = NULL;
    node->list.length = 0;

    (*buffer_pos)++; // Move past list open token
    while (true) {
        ParseResult result = seek_expression(
            file_buffer, buffer_pos, &exp_type);

        if (result == LP_PARSE_RESULT_ERROR) {
            return result;
        }

        switch (exp_type) {
        case LP_PARSE_EXP_NONE:
            fprintf(stderr, "parse_list: expression not found\n");

            return LP_PARSE_RESULT_ERROR;
            break;

        case LP_PARSE_EXP_LIST_END:
            printf("parse_list: found: )\n");
            (*buffer_pos)++;

            return LP_PARSE_RESULT_OK;
            break;

        case LP_PARSE_EXP_ATOM:
            printf("parse_list: found: atom\n");
            Node* new_atom_node = append_list_node(node);
            ParseResult atom_result = parse_token_atom(file_buffer, buffer_pos, new_atom_node);

            if (atom_result != LP_PARSE_RESULT_OK) {
                return atom_result;
            }

            break;
        case LP_PARSE_EXP_LIST_START:
            printf("parse_list: found: (\n");

            Node* new_list_node = append_list_node(node);
            new_list_node->node_type = LP_NODE_LIST;
            new_list_node->list.length = 0;

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
    ParseExpressionType exp_type;
    int buffer_pos = 0;

    ParseResult result = seek_expression(
        exp, &buffer_pos, &exp_type);

    if (result == LP_PARSE_RESULT_ERROR) {
        return result;
    }
    if (exp_type != LP_PARSE_EXP_LIST_START) {
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
