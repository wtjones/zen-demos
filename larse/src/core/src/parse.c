#include "larse/core/parse.h"
#include "io.h"
#include "larse/core/expression.h"
#include "log.c/src/log.h"
#include "parse_token.h"
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
    char ch = file_buffer[(*buffer_pos)];

    do {
        while ('\0' != ch && is_whitespace_char(ch)) {
            (*buffer_pos)++;
            ch = file_buffer[(*buffer_pos)];
        }

        if ('\0' == ch) {
            *exp_type = LAR_PARSE_EXP_NONE;
            return LAR_PARSE_RESULT_OK;
        }

        // If comment, seek to EOL and restart the search
        if (';' == ch) {
            while ('\0' != ch && ch != '\n') {
                (*buffer_pos)++;
                ch = file_buffer[(*buffer_pos)];
            }
            continue;
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

    } while (true);

    assert(false);
    return LAR_PARSE_RESULT_ERROR;
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
            log_error("parse_list: expression not found\n");

            return LAR_PARSE_RESULT_ERROR;
            break;

        case LAR_PARSE_EXP_LIST_END:
            log_info("parse_list: found: )", "");
            (*buffer_pos)++;

            return LAR_PARSE_RESULT_OK;
            break;

        case LAR_PARSE_EXP_ATOM:
            log_info("parse_list: found: atom", "");
            LarNode* new_atom_node = append_list_node(node);
            new_atom_node->node_type = LAR_NODE_ATOM_INTEGER;
            LarParseResult atom_result = parse_token_atom(file_buffer, buffer_pos, new_atom_node);

            if (atom_result != LAR_PARSE_RESULT_OK) {
                return atom_result;
            }

            break;
        case LAR_PARSE_EXP_LIST_START:
            log_info("parse_list: found: (", "");

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
        log_error("Expression not found.\n");
        return LAR_PARSE_RESULT_ERROR;
    }

    *node = malloc(sizeof(LarNode));

    if (*node == NULL) {
        log_error("Cannot malloc()\n");
        return LAR_PARSE_RESULT_ERROR;
    }

    LarParseResult exp_result = parse_list(
        exp, &buffer_pos, *node, 1);

    if (exp_result == LAR_PARSE_RESULT_ERROR) {
        return exp_result;
    }

    return LAR_PARSE_RESULT_OK;
}

LarParseResult lar_parse_file(
    const char* file_path, LarScript** script)
{
    char* file_buffer = io_read_file(file_path);
    if (file_buffer == NULL) {
        return LAR_PARSE_RESULT_ERROR;
    }

    LarParseResult result = lar_parse_script(file_buffer, script);

    free(file_buffer);

    return result;
}

LarParseResult lar_parse_script(
    const char* exp, LarScript** script)
{
    int buffer_pos = 0;
    LarScript* new_script = malloc(sizeof(LarScript));
    if (new_script == NULL) {
        log_error("Error from malloc().\n");
        return LAR_PARSE_RESULT_ERROR;
    }

    new_script->expressions = malloc(sizeof(LarNode));
    if (new_script->expressions == NULL) {
        log_error("Error from malloc().\n");
        free(new_script);
        return LAR_PARSE_RESULT_ERROR;
    }

    new_script->expressions->node_type = LAR_NODE_LIST;
    new_script->expressions->list.length = 0;

    LarParseExpressionType exp_type;

    LarParseResult result = seek_expression(
        exp, &buffer_pos, &exp_type);

    while (exp_type != LAR_PARSE_EXP_NONE) {

        if (exp_type != LAR_PARSE_EXP_LIST_START) {
            log_error("Expression not found.\n");
            // TODO: free
            return LAR_PARSE_RESULT_ERROR;
        }

        LarNode* new_list_node = append_list_node(new_script->expressions);
        new_list_node->node_type = LAR_NODE_LIST;
        new_list_node->list.length = 0;

        LarParseResult list_result = parse_list(
            exp, &buffer_pos, new_list_node, 1);

        if (list_result == LAR_PARSE_RESULT_ERROR) {
            // TODO: free
            return list_result;
        }

        LarParseResult result = seek_expression(
            exp, &buffer_pos, &exp_type);
    }

    *script = new_script;
    return LAR_PARSE_RESULT_OK;
}
