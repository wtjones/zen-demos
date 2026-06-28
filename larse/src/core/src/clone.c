
#include "larse/core/clone.h"
#include "larse/core/repr.h"
#include "log.c/src/log.h"
#include <assert.h>

// forward declare
bool lar_clone_expression_walk(LarNode* dest, const LarNode* src, int depth);

bool lar_clone_list(LarNode* dest, const LarNode* src, int depth)
{
    if (depth > 10) {
        log_error("Max depth reached.");
        return false;
    }
    dest->node_type = LAR_NODE_LIST;
    dest->list.length = 0;
    assert(dest->list.nodes == NULL);

    char* buffer = lar_repr_expression(src);
    log_info("Cloning from src: %s", buffer);
    free(buffer);
    for (size_t i = 0; i < src->list.length; i++) {
        LarNode* src_item = &src->list.nodes[i];
        buffer = lar_repr_expression(dest);
        log_info("Appending to dest: %s", buffer);
        free(buffer);
        LarNode* dest_item = lar_append_list_node(dest);
        if (!dest_item) {
            log_error("Unable to append list");
            return false;
        }

        bool result = lar_clone_expression_walk(dest_item, src_item, depth);
        if (!result) {
            return false;
        }
        buffer = lar_repr_expression(dest);
        log_info("Appended to dest: %s", buffer);
        free(buffer);
    }
    return true;
}

bool lar_clone_atom(LarNode* dest, const LarNode* src)
{

    if (src->node_type == LAR_NODE_LIST) {
        log_error("Invalid node type.");
        return false;
    }

    switch (src->node_type) {

    case LAR_NODE_ATOM_SYMBOL:
        dest->node_type = src->node_type;
        dest->atom.val_symbol = malloc(strlen(src->atom.val_symbol) + 1);
        assert(dest->atom.val_symbol);
        strcpy(dest->atom.val_symbol, src->atom.val_symbol);
        break;

    case LAR_NODE_ATOM_STRING:

        dest->node_type = src->node_type;
        dest->atom.val_string = malloc(strlen(src->atom.val_string) + 1);
        assert(dest->atom.val_string);
        strcpy(dest->atom.val_string, src->atom.val_string);
        break;

    default:
        memcpy(dest, src, sizeof(LarNode));
        break;
    }
    return true;
}

bool lar_clone_expression_walk(LarNode* dest, const LarNode* src, int depth)
{

    switch (src->node_type) {
    case LAR_NODE_LIST:
        return lar_clone_list(dest, src, depth + 1);
        break;

    default:
        return lar_clone_atom(dest, src);
    }
}

bool lar_clone_expression(LarNode* dest, const LarNode* src)
{
    return lar_clone_expression_walk(dest, src, 1);
}
