#include "larse/core/merge.h"
#include "larse/core/clone.h"
#include "log.c/src/log.h"
#include <assert.h>

typedef enum LarMergeBehavior {
    LAR_MERGE_LIST,
    LAR_MERGE_MAP,
    LAR_MERGE_CLONE0,
    LAR_MERGE_CLONE1,
    LAR_MERGE_PROPERTY,
    LAR_MERGE_ARRAY,
    LAR_MERGE_SKIP
} LarMergeBehavior;

/**
 * @brief Is node a symbol formatted as a :property?
 *
 * @param node
 * @return true
 * @return false
 */
bool is_property(const LarNode* node)
{
    if (node->node_type != LAR_NODE_ATOM_SYMBOL) {
        return false;
    }
    if (strlen(node->atom.val_symbol) < 2 || node->atom.val_symbol[0] != ':') {
        return false;
    }
    return true;
}
LarNode* get_head_symbol(const LarNode* node)
{
    LarNode* item_head = lar_get_first(node);

    if (item_head == NULL || item_head->node_type != LAR_NODE_ATOM_SYMBOL) {
        return NULL;
    }
    return item_head;
}

bool can_merge_map(const LarNode* node)
{
    if (node->node_type != LAR_NODE_LIST) {
        log_error("Expected list.");
        return false;
    }
    if (node->list.length == 0) {
        return false;
    }
    // FIXME: support list with head symbol
    const char* check = NULL;
    for (size_t i = 0; i < node->list.length; i++) {
        LarNode* item = &node->list.nodes[i];

        if (item->node_type != LAR_NODE_LIST) {
            return false;
        }
        LarNode* item_head = lar_get_first(item);

        if (item_head->node_type != LAR_NODE_ATOM_SYMBOL) {
            return false;
        }

        if (!check) {
            check = item_head->atom.val_symbol;
        } else {
            if (strcmp(check, item_head->atom.val_symbol) == 0) {
                log_info("Duplicate head symbol found, cannot merge map.");
                return false;
            }
        }
    }
    return true;
}

/**
 * @brief Determine if list is an 'object' - has a head + properties.
 *
 * @param node
 * @return true
 * @return false
 */
bool can_merge_properties(const LarNode* node)
{
    if (node->node_type != LAR_NODE_LIST) {
        log_error("Expected list.");
        return false;
    }

    if (!get_head_symbol(node)) {
        log_info("No head symbol. Cannot merge properties.");
        return false;
    }
    if (node->list.length / 2 == 0) {
        log_info("Property count mismatch.");
        return false;
    }

    // Is object?
    // Iterate every other item after the head.

    for (size_t i = 1; i < node->list.length; i += 2) {
        const LarNode* item = &node->list.nodes[i];
        const LarNode* item_next = &node->list.nodes[i + 1];

        if (!is_property(item)) {
            log_info("Not symbol or formatted as a property.");
            return false;
        }
        if (is_property(item_next)) {
            log_info("Element following property should not be a property");
            return false;
        }
    }
    return true;
}

LarMergeBehavior get_merge_behavior(const LarNode* src0, const LarNode* src1)
{
    if (src0->node_type != LAR_NODE_LIST) {
        log_error("Expected list.");
        return LAR_MERGE_SKIP;
    }

    // call this merge object?
    if (can_merge_properties(src0)) {
        if (can_merge_properties(src1)) {
            return LAR_MERGE_PROPERTY;
        } else {
            return LAR_MERGE_CLONE0;
        }
    }

    if (can_merge_map(src0)) {
        if (can_merge_map(src1)) {
            return LAR_MERGE_MAP;
        } else {
            return LAR_MERGE_CLONE0;
        }
    }

    return LAR_MERGE_SKIP;
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
    case LAR_NODE_ATOM_INTEGER:
        new_node->node_type = LAR_NODE_ATOM_INTEGER;
        new_node->atom.val_integer = to_copy->atom.val_integer;
        break;
    }
    return new_node;
}

void lar_merge_object(LarNode* dest, const LarNode* src0, const LarNode* src1, int depth)
{
    // heads should match
    LarNode* head0 = get_head_symbol(src0);
    LarNode* head1 = get_head_symbol(src1);
    if (!head0 && !head1) {
        log_error("Invalid merge.");
        return;
    }

    // Add head to dest
    LarNode* new_node = NULL;
    new_node = lar_append_list_node(dest);
    assert(lar_clone_expression(new_node, head0));

    // Add properties from src0.
    // Backfill if found in src1.
    for (size_t i = 1; i < src0->list.length; i += 2) {
        LarNode* src0_item = &src0->list.nodes[i];
        const char* property_name = is_property(src0_item) && i < src0->list.length - 1
            ? src0_item->atom.val_symbol
            : NULL;
        assert(property_name);
        LarNode* src0_value = lar_get_property(src0, property_name);
        LarNode* src1_value = lar_get_property(src1, property_name);
        LarNode* dest_value = src1_value == NULL ? src0_value : src1_value;
        assert(dest_value);

        LarNode* new_node = NULL;
        new_node = lar_append_list_node(dest);
        lar_clone_expression(new_node, src0_item);
        new_node = lar_append_list_node(dest);
        lar_clone_expression(new_node, dest_value);
    }

    // Backfill properties from src1 that do not exist in src0.
    for (size_t i = 1; i < src1->list.length; i += 2) {
        LarNode* src1_item = &src1->list.nodes[i];
        const char* property_name = is_property(src1_item) && i < src1->list.length - 1
            ? src1_item->atom.val_symbol
            : NULL;
        assert(property_name);
        LarNode* src0_value = lar_get_property(src0, property_name);
        LarNode* src1_value = lar_get_property(src1, property_name);
        if (!src0_value) {
            LarNode* new_node = NULL;
            new_node = lar_append_list_node(dest);
            lar_clone_expression(new_node, src1_item);
            new_node = lar_append_list_node(dest);
            lar_clone_expression(new_node, src1_value);
        }
    }
}

void lar_merge_map(LarNode* dest, const LarNode* src0, const LarNode* src1, int depth)
{

    for (size_t i = 0; i < src0->list.length; i++) {
        LarNode* item0 = &src0->list.nodes[i];
        LarNode* head0 = get_head_symbol(item0);

        // Find list in src1 with matching head symbol.
        LarNode* item1 = NULL;
        for (size_t j = 0; j < src1->list.length; j++) {
            LarNode* temp1 = &src1->list.nodes[j];
            if (lar_get_symbol_index(temp1, head0->atom.val_symbol) == 0) {
                item1 = temp1;
                break;
            }
        }
        if (!item1) {
            // Add to dest
            LarNode* new_node = NULL;
            new_node = lar_append_list_node(dest);
            lar_clone_expression(new_node, item0);
            continue;
        }

        // Add an empty list to dest
        LarNode* new_node = NULL;
        new_node = lar_append_list_node(dest);
        new_node->node_type = LAR_NODE_LIST;
        new_node->list.length = 0;

        lar_merge_list(new_node, item0, item1, depth + 1);
    }

    // Append lists from src1 not found in src0.
    for (size_t i = 0; i < src1->list.length; i++) {
        LarNode* item1 = &src1->list.nodes[i];
        LarNode* head1 = get_head_symbol(item1);

        LarNode* item0 = NULL;
        for (size_t j = 0; j < src0->list.length; j++) {
            LarNode* temp0 = &src0->list.nodes[j];
            if (lar_get_symbol_index(temp0, head1->atom.val_symbol) == 0) {
                item0 = temp0;
                break;
            }
        }
        if (!item0) {
            // Add to dest
            LarNode* new_node = NULL;
            new_node = lar_append_list_node(dest);
            lar_clone_expression(new_node, item1);
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
void lar_merge_list(LarNode* dest, const LarNode* src0, const LarNode* src1, int depth)
{
    if (depth > 10) {
        log_error("Max depth reached.");
        return;
    }

    if (src0->node_type != LAR_NODE_LIST) {
        log_error("Input node must be list.");
        return;
    }

    LarMergeBehavior behavior = get_merge_behavior(src0, src1);

    switch (behavior) {
    case LAR_MERGE_PROPERTY:
        log_info("Behavior: LAR_MERGE_PROPERTY");
        lar_merge_object(dest, src0, src1, depth);
        break;
    case LAR_MERGE_MAP:
        log_info("Behavior: LAR_MERGE_MAP");
        lar_merge_map(dest, src0, src1, depth);
        break;
    case LAR_MERGE_CLONE0:
        log_info("Behavior: LAR_MERGE_CLONE0");
        lar_clone_expression(dest, src0);
        break;
    default:
        log_error("Behavior: unsupported");
        assert(false);
        break;
    }
}

void lar_merge_script(LarScript* dest, LarScript* src0, LarScript* src1)
{

    dest->expressions = calloc(1, sizeof(LarNode));
    dest->expressions->node_type = LAR_NODE_LIST;

    lar_merge_list(dest->expressions, src0->expressions, src1->expressions, 0);
}
