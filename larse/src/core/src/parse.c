#include "larse/core/parse.h"
#include "larse/core/expression.h"
#include "larse/core/parse_token.h"
#include <assert.h>
#include <stdio.h>

LarNode* append_list_node(LarNode* node)
{
    if (node->list.length == 0) {
        node->list.length = 1;
        node->list.nodes = malloc(sizeof(LarNode) * node->list.length);
    } else {
        node->list.length++;
        node->list.nodes = realloc(node->list.nodes, sizeof(LarNode) * node->list.length);
    }
    return &node->list.nodes[node->list.length - 1];
}

bool is_whitespace_char(char ch)
{
    char search_chars[] = " \n";
    return strchr(search_chars, ch) != NULL;
}

/**
 * @brief Seek to next token and identify next top-level expression type
 *
 * @param file_buffer
 * @param buffer_pos
 * @param token_type
 * @return LarParseResult
 */
LarParseResult seek_expression(
    const char* file_buffer,
    int* buffer_pos,
    LarParseExpressionType* exp_type)
{
    // Find first non-whitespace
    char ch = file_buffer[(*buffer_pos)];
    while ('\0' != ch && is_whitespace_char(ch)) {
        (*buffer_pos)++;
        ch = file_buffer[(*buffer_pos)];
    }

    if ('\0' == ch) {
        *exp_type = LAR_PARSE_EXP_NONE;
        return LAR_PARSE_RESULT_OK;
    }

    if ('(' == ch) {
        *exp_type = LAR_PARSE_EXP_LIST_START;

        return LAR_PARSE_RESULT_OK;
    }

    if (')' == ch) {
        *exp_type = LAR_PARSE_EXP_LIST_END;
        return LAR_PARSE_RESULT_OK;
    }

    *exp_type = LAR_PARSE_EXP_ATOM;
    return LAR_PARSE_RESULT_OK;
}

LarParseResult parse_list(
    const char* file_buffer,
    int* buffer_pos,
    LarNode* node,
    int depth)
{
    LarParseExpressionType exp_type;

    assert(depth < LAR_PARSE_DEPTH_MAX);

    node->node_type = LAR_NODE_LIST;
    node->list.nodes = NULL;
    node->list.length = 0;

    (*buffer_pos)++; // Move past list open token
    while (true) {
        LarParseResult result = seek_expression(
            file_buffer, buffer_pos, &exp_type);

        if (result == LAR_PARSE_RESULT_ERROR) {
            return result;
        }

        switch (exp_type) {
        case LAR_PARSE_EXP_NONE:
            fprintf(stderr, "parse_list: expression not found\n");

            return LAR_PARSE_RESULT_ERROR;
            break;

        case LAR_PARSE_EXP_LIST_END:
            printf("parse_list: found: )\n");
            (*buffer_pos)++;

            return LAR_PARSE_RESULT_OK;
            break;

        case LAR_PARSE_EXP_ATOM:
            printf("parse_list: found: atom\n");
            LarNode* new_atom_node = append_list_node(node);
            new_atom_node->node_type = LAR_NODE_ATOM_INTEGER;
            LarParseResult atom_result = parse_token_atom(file_buffer, buffer_pos, new_atom_node);

            if (atom_result != LAR_PARSE_RESULT_OK) {
                return atom_result;
            }

            break;
        case LAR_PARSE_EXP_LIST_START:
            printf("parse_list: found: (\n");

            LarNode* new_list_node = append_list_node(node);
            new_list_node->node_type = LAR_NODE_LIST;
            new_list_node->list.length = 0;

            LarParseResult list_result = parse_list(
                file_buffer, buffer_pos, new_list_node, depth + 1);

            if (list_result == LAR_PARSE_RESULT_ERROR) {
                return list_result;
            }

            break;

        default:
            break;
        }
    }

    return LAR_PARSE_RESULT_OK;
}

LarParseResult lar_parse_single(
    const char* exp, LarNode** node)
{

    LarParseExpressionType exp_type;
    int buffer_pos = 0;

    LarParseResult result = seek_expression(
        exp, &buffer_pos, &exp_type);

    if (result == LAR_PARSE_RESULT_ERROR) {
        return result;
    }
    if (exp_type != LAR_PARSE_EXP_LIST_START) {
        fprintf(stderr, "Expression not found.\n");
        return LAR_PARSE_RESULT_ERROR;
    }

    *node = malloc(sizeof(LarNode));

    if (*node == NULL) {
        fprintf(stderr, "Cannot malloc()\n");
        return LAR_PARSE_RESULT_ERROR;
    }

    LarParseResult exp_result = parse_list(
        exp, &buffer_pos, *node, 1);

    if (exp_result == LAR_PARSE_RESULT_ERROR) {
        return exp_result;
    }

    return LAR_PARSE_RESULT_OK;
}

LarParseResult lar_parse(
    const char* exp, LarExpressions* expressions)
{

    expressions->count = 1;

    // Allocate initial expression array size
    expressions->nodes = malloc(sizeof(LarNode*) * LAR_PARSE_EXPRESSIONS_MAX);

    if (expressions->nodes == NULL) {
        return LAR_PARSE_RESULT_ERROR;
    }

    expressions->nodes[0] = malloc(sizeof(LarNode));
    if (expressions->nodes[0] == NULL) {
        return LAR_PARSE_RESULT_ERROR;
    }

    // FIXME
    fprintf(stderr, "Error: Function not implemented yet\n");
    abort();

    return LAR_PARSE_RESULT_OK;
}
