#include "larse/core/expression.h"
#include "log.c/src/log.h"
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
        log_error("Invalid node type", "");
        assert(false);
        break;
    }
}

void lar_free_expression(LarNode** node)
{
    if (*node == NULL) {
        return;
    }
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

LarNode* lar_get_list_by_symbol(const LarNode* list, const char* symbol)
{
    if (list->node_type != LAR_NODE_LIST) {
        return NULL;
    }

    for (size_t i = 0; i < list->list.length; i++) {
        LarNode* node_item = &list->list.nodes[i];
        if (node_item->node_type == LAR_NODE_LIST) {
            LarNode* first_node = lar_get_first(node_item);
            if (first_node != NULL
                && first_node->node_type == LAR_NODE_ATOM_SYMBOL
                && strcmp(first_node->atom.val_symbol, symbol) == 0) {
                return node_item;
            }
        }
    }
    return NULL;
}

LarNode* lar_get_property(const LarNode* list, const char* property_name)
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
    return &list->list.nodes[symbol_index + 1];
}

LarNode* lar_get_property_by_type(
    const LarNode* list, const char* property_name, LarNodeType type)
{
    LarNode* node = lar_get_property(list, property_name);
    if (node == NULL || node->node_type != type) {
        return NULL;
    }
    return node;
}

LarNode* lar_get_list_node_by_index(const LarNode* list, size_t index)
{
    if (list->node_type != LAR_NODE_LIST || index >= list->list.length) {
        return NULL;
    }
    return &list->list.nodes[index];
}

bool lar_is_symbol(const LarNode* node, const char* symbol)
{
    return node != NULL
        && node->node_type == LAR_NODE_ATOM_SYMBOL
        && strcmp(node->atom.val_symbol, symbol) == 0;
}

LarNode* lar_append_list_node(LarNode* node)
{
    LarNode* temp = NULL;
    if (node->list.nodes == NULL) {
        node->list.length = 1;
        temp = calloc(node->list.length, sizeof(LarNode));
    } else {
        node->list.length++;
        temp = realloc(node->list.nodes, sizeof(LarNode) * node->list.length);
    }
    if (temp == NULL) {
        return NULL;
    }
    node->list.nodes = temp;
    LarNode* new_node = &node->list.nodes[node->list.length - 1];
    memset(new_node, 0, sizeof(LarNode));
    return new_node;
}

/**
 * @brief Add an element to the provided list node and copy the provided atom.
 *
 * @param to_append
 * @param to_copy
 * @return LarNode*
 */
LarNode* append_list_atom(LarNode* to_append, LarNode* to_copy)
{
    LarNode* new_node = lar_append_list_node(to_append);

    switch (to_copy->node_type) {

    case LAR_NODE_ATOM_SYMBOL:
        new_node->node_type = LAR_NODE_ATOM_SYMBOL;
        new_node->atom.val_symbol = malloc(strlen(to_copy->atom.val_symbol) + 1);
        strcpy(new_node->atom.val_symbol, to_copy->atom.val_symbol);
        break;

    case LAR_NODE_ATOM_STRING:
        log_info("Copy string %s", to_copy->atom.val_string);

        new_node->node_type = LAR_NODE_ATOM_STRING;
        new_node->atom.val_string = malloc(strlen(to_copy->atom.val_string) + 1);
        strcpy(new_node->atom.val_string, to_copy->atom.val_string);
        break;
        return new_node;
    }
}

/**
 * @brief
 *
 * @param src0
 * @param src1
 * @param dest
 */
void lar_merge_list_walk(const LarNode* src0, const LarNode* src1, LarNode** dest, uint32_t depth)
{
    if (depth > 10) {
        log_info("Reached max recurse.");
        return;
    }
    LarNode* dest_list = (*dest);

    for (size_t i = 0; i < src0->list.length; i++) {
        LarNode* node = &src0->list.nodes[i];
        LarNode* new_node = NULL;
        switch (node->node_type) {

        case LAR_NODE_ATOM_SYMBOL:
        case LAR_NODE_ATOM_STRING:
            // fixme: copy until symbol or end
            new_node = append_list_atom(dest_list, node);
            break;

        case LAR_NODE_LIST:
            if (node->list.length > 0
                && node->list.nodes[0].node_type == LAR_NODE_ATOM_SYMBOL) {

                LarNode* head = &node->list.nodes[0];
                log_info("List 1: Searching for list node with head %s", head->atom.val_symbol);
                // Find a list in src1 with the same head.
                LarNode* src1_list = NULL;
                for (size_t j = 0; j < src1->list.length; j++) {
                    // Is it a list?
                    src1_list = src1->list.nodes[j].node_type == LAR_NODE_LIST
                        ? &src1->list.nodes[j]
                        : NULL;
                    // Is the head a symbol?
                    src1_list = src1_list
                            && src1_list->list.length > 0
                            && src1_list->list.nodes[0].node_type == LAR_NODE_ATOM_SYMBOL
                        ? src1_list
                        : NULL;
                    // Is it the same symbol?
                    src1_list = src1_list
                            && strcmp(src1_list->list.nodes[0].atom.val_symbol, head->atom.val_symbol) == 0
                        ? src1_list
                        : NULL;
                    if (src1_list) {
                        break;
                    }
                }

                if (src1_list) {
                    log_info("List 1: Found match, recurse");
                    // TODO: alloc dest
                    lar_merge_list_walk(node, src1_list, dest, depth + 1);
                } else {
                    log_info("List 1: List Node not found.");
                }
            }
        }
    }
}

/**
 * @brief
 *
 * @param src0
 * @param src1
 * @param dest
 */
void lar_merge_list(const LarNode* src0, const LarNode* src1, LarNode** dest)
{

    /*
        loop through src0 list nodes
            if node is list:
                append to dest
                search src1 list
                if found:
                    recurse
                if not found:
                    clone to dest
            if node is symbol:
                append to dest
                search src1 list for symbol
                if found:
                    add nodes from src1 until symbol or end
                if not found:
                    add nodes from src0 until symbol or end
    */
    if (src0->node_type != LAR_NODE_LIST || src1->node_type != LAR_NODE_LIST) {
        log_error("Input nodes must be list.");
        return;
    }
    (*dest) = calloc(1, sizeof(LarNode));
    (*dest)->node_type = LAR_NODE_LIST;

    lar_merge_list_walk(src0, src1, dest, 0);
}

void lar_merge_script(LarScript* src0, LarScript* src1, LarScript* dest)
{
    lar_merge_list(src0->expressions, src1->expressions, &dest->expressions);
}
