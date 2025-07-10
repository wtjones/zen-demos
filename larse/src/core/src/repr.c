
#include "larse/core/repr.h"
#include <assert.h>
#include <stdio.h>

float fixed_to_float(LarFixed n)
{

    int32_t whole = n / 65536;
    int32_t fixed_frac = n - (whole * 65536);
    return (float)(whole + (float)(fixed_frac) / 65536);
}

char* strcat_alloc(char* str, const char* append)
{
    size_t old_size = str == NULL
        ? 0
        : strlen(str);

    char* result = realloc(str, old_size + strlen(append) + 1);

    result[0] = old_size == 0 ? '\0' : result[0];
    return strcat(result, append);
}

char* repr_expression_walk(LarNode* node, char* work, char* buffer, int depth)
{
    char* result = work;
    char* buffer_append = buffer;
    buffer[0] = '\0';

    assert(depth <= LAR_PARSE_DEPTH_MAX);

    switch (node->node_type) {
    case LAR_NODE_LIST:

        for (int i = 0; i < depth * LAR_REPR_INDENT; i++) {
            buffer_append += snprintf(
                buffer_append, LAR_REPR_MAX_LINE - (int)(buffer_append - buffer), " ");
        }

        buffer_append += snprintf(
            buffer_append, LAR_REPR_MAX_LINE - (int)(buffer_append - buffer), "%s\n", LAR_REPR_LIST);
        result = strcat_alloc(result, buffer);

        for (size_t i = 0; i < node->list.length; i++) {
            LarNode* list_item = &node->list.nodes[i];

            result = repr_expression_walk(list_item, result, buffer, depth + 1);
            if (node->list.length > 1 && i != node->list.length - 1) {
                result = strcat_alloc(result, "\n");
            }
        }
        break;

    case LAR_NODE_ATOM_SYMBOL:
        for (int i = 0; i < depth * LAR_REPR_INDENT; i++) {
            buffer_append += snprintf(
                buffer_append, LAR_REPR_MAX_LINE - (int)(buffer_append - buffer), " ");
        }
        buffer_append += snprintf(
            buffer_append,
            LAR_REPR_MAX_LINE - (int)(buffer_append - buffer),
            "%s %s", node->atom.val_symbol, LAR_REPR_ATOM_SYMBOL);
        result = strcat_alloc(result, buffer);
        break;

    case LAR_NODE_ATOM_STRING:
        for (int i = 0; i < depth * LAR_REPR_INDENT; i++) {
            buffer_append += snprintf(
                buffer_append,
                LAR_REPR_MAX_LINE - (int)(buffer_append - buffer),
                " ");
        }
        buffer_append += snprintf(
            buffer_append,
            LAR_REPR_MAX_LINE - (int)(buffer_append - buffer),
            "\"%s\" %s", node->atom.val_string, LAR_REPR_ATOM_STRING);
        result = strcat_alloc(result, buffer);
        break;

    case LAR_NODE_ATOM_BOOLEAN:
        for (int i = 0; i < depth * LAR_REPR_INDENT; i++) {
            buffer_append += snprintf(buffer_append,
                LAR_REPR_MAX_LINE - (int)(buffer_append - buffer),
                " ");
        }
        buffer_append += snprintf(
            buffer_append,
            LAR_REPR_MAX_LINE - (int)(buffer_append - buffer),
            "%s %s", node->atom.val_bool ? "true" : "false", LAR_REPR_ATOM_BOOLEAN);
        result = strcat_alloc(result, buffer);
        break;

    case LAR_NODE_ATOM_INTEGER:
        for (int i = 0; i < depth * LAR_REPR_INDENT; i++) {
            buffer_append += snprintf(buffer_append,
                LAR_REPR_MAX_LINE - (int)(buffer_append - buffer),
                " ");
        }
        buffer_append += snprintf(
            buffer_append,
            LAR_REPR_MAX_LINE - (int)(buffer_append - buffer),
            "%d %s", node->atom.val_integer, LAR_REPR_ATOM_INTEGER);
        result = strcat_alloc(result, buffer);
        break;

    case LAR_NODE_ATOM_FIXED:
        for (int i = 0; i < depth * LAR_REPR_INDENT; i++) {
            buffer_append += snprintf(buffer_append,
                LAR_REPR_MAX_LINE - (int)(buffer_append - buffer),
                " ");
        }
        buffer_append += snprintf(
            buffer_append,
            LAR_REPR_MAX_LINE - (int)(buffer_append - buffer),
            "%f %s", fixed_to_float(node->atom.val_fixed), LAR_REPR_ATOM_FIXED);
        result = strcat_alloc(result, buffer);
        break;
    }
    return result;
}

char* lar_repr_expression(LarNode* node)
{
    char* buffer = malloc(LAR_REPR_MAX_LINE);
    if (buffer == NULL) {
        return NULL;
    }
    char* result = repr_expression_walk(node, (char*)NULL, buffer, 0);
    free(buffer);
    return result;
}

char* lar_repr_script(LarScript* script)
{
    char* result = NULL;
    char* buffer = malloc(LAR_REPR_MAX_LINE);
    if (buffer == NULL) {
        return NULL;
    }

    result = strcat_alloc(result, LAR_REPR_SCRIPT);
    result = strcat_alloc(result, "\n");

    for (size_t i = 0; i < script->expressions->list.length; i++) {
        LarNode* node = &script->expressions->list.nodes[i];
        result = repr_expression_walk(node, result, buffer, 1);
        if (script->expressions->list.length > 1 && i != script->expressions->list.length - 1) {
            result = strcat_alloc(result, "\n");
        }
    }

    free(buffer);
    return result;
}
