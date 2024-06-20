#include "larse/core/expression.h"
#include <assert.h>
#include <stdio.h>

void free_expression_walk(LarNode* node, int depth)
{
    assert(depth <= LAR_PARSE_DEPTH_MAX);

    if (node == NULL) {
        return;
    }

    switch (node->node_type) {
    case LAR_NODE_LIST:
        for (size_t i = 0; i < node->list.length; i++) {
            LarNode* node_item = &node->list.nodes[i];
            free_expression_walk(node_item, depth + 1);
        }
        free(node->list.nodes);
        break;

    case LAR_NODE_ATOM_SYMBOL:
        free(node->atom.val_symbol);
        break;

    case LAR_NODE_ATOM_STRING:
        free(node->atom.val_string);
        break;

    case LAR_NODE_ATOM_INTEGER:
    case LAR_NODE_ATOM_FIXED:
    case LAR_NODE_ATOM_BOOLEAN:
        break;

    default:
        fprintf(stderr, "Invalid node type\n");
        assert(false);
        break;
    }
}

void lar_free_expression(LarNode** node)
{
    free_expression_walk(*node, 0);
    free(*node);
    *node = NULL;
}

void lar_free_script(LarScript** script)
{
    lar_free_expression(&(*script)->expressions);
    free(*script);
    *script = NULL;
}

LarNode* lar_get_first(const LarNode* list)
{
    if (list->node_type != LAR_NODE_LIST || list->list.length == 0) {
        return NULL;
    }
    return &list->list.nodes[0];
}

LarNode* lar_get_string_property(
    const LarNode* list, const char* property_name)
{
    LarNode* symbol_node = NULL;
    bool found_symbol = false;
    size_t symbol_index = 0;
    for (size_t i = 0; i < list->list.length; i++) {
        LarNode* node_item = &list->list.nodes[i];
        if (node_item->node_type == LAR_NODE_ATOM_SYMBOL
            && strcmp(node_item->atom.val_symbol, property_name) == 0) {
            symbol_node = node_item;
            found_symbol = true;
            symbol_index = i;
            break;
        }
    }
    if (!found_symbol) {
        return NULL;
    }
    if (symbol_index + 1 >= list->list.length) {
        return NULL;
    }
    LarNode* next_node = &list->list.nodes[symbol_index + 1];
    if (next_node->node_type != LAR_NODE_ATOM_STRING) {
        return NULL;
    }
    return next_node;
}
