#include "parse_token.h"
#include "log.c/src/log.h"

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

LarParseResult parse_token_atom_boolean(const char* file_buffer,
    int* buffer_pos,
    LarNode* node)
{
    char token[LAR_PARSE_TOKEN_MAX] = "";
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
        return LAR_PARSE_RESULT_PASS;
    }

    log_debug("Boolean found, adding to node: %s\n", is_true ? "true" : "false");
    node->node_type = LAR_NODE_ATOM_BOOLEAN;
    node->atom.val_bool = is_true;
    *buffer_pos = pos;

    return LAR_PARSE_RESULT_OK;
}

LarParseResult parse_token_atom_integer(
    const char* file_buffer,
    int* buffer_pos,
    LarNode* node)
{
    char token[LAR_PARSE_TOKEN_MAX] = "";
    char ch[2] = "";
    int pos = *buffer_pos;
    char sign;

    ch[0] = file_buffer[pos];
    assert(ch[0] != '\0');

    bool has_sign = (ch[0] == '+' || ch[0] == '-');
    sign = has_sign ? ch[0] : '+';

    if (has_sign) {
        pos++;
        ch[0] = file_buffer[pos];
    }

    // Treat as atom and copy to token
    size_t num_digits = 0, num_decimal_points = 0;
    while ('\0' != ch[0] && !is_atom_end_char(ch[0])) {
        num_digits += isdigit(ch[0]) ? 1 : 0;
        num_decimal_points += ch[0] == '.' ? 1 : 0;
        strcat(token, ch);
        pos++;
        ch[0] = file_buffer[pos];
    }

    bool is_integer = num_digits > 0 && num_digits == strlen(token);

    if (!is_integer) {
        log_debug("parse_token_atom_number(): token %s not int, passing...\n", token);
        return LAR_PARSE_RESULT_PASS;
    }

    node->node_type = LAR_NODE_ATOM_INTEGER;
    node->atom.val_integer = atoi(token) * (sign == '-' ? -1 : 1);
    log_debug("Integer found, adding to node: %d\n", node->atom.val_integer);
    (*buffer_pos) = pos;

    return LAR_PARSE_RESULT_OK;
}

LarFixed decimal_to_fixed(char sign, char* whole, char* frac)
{
    log_info("fractional part: %s, whole part: %s\n", frac, whole);

    int64_t exp = pow(10, (int)strlen(frac));
    int64_t frac_part = (int64_t)atoi(frac) * INT64_C(65536) / exp;

    log_debug("Fractional shifted: %lld\n", frac_part);
    LarFixed result = (int64_t)atoi(whole) * INT64_C(65536) + frac_part;

    result *= (sign == '+' ? 1 : -1);
    return result;
}

LarParseResult parse_token_atom_fixed(
    const char* file_buffer,
    int* buffer_pos,
    LarNode* node)
{
    char token[LAR_PARSE_TOKEN_MAX] = "";
    char whole[LAR_PARSE_TOKEN_MAX] = "";
    char frac[LAR_PARSE_TOKEN_MAX] = "";

    char ch[2] = "";
    int pos = *buffer_pos;
    char sign;

    ch[0] = file_buffer[pos];
    assert(ch[0] != '\0');

    bool has_sign = (ch[0] == '+' || ch[0] == '-');
    sign = has_sign ? ch[0] : '+';

    if (has_sign) {
        pos++;
        ch[0] = file_buffer[pos];
    }

    // Treat as atom and copy to token
    size_t num_digits = 0,
           num_decimal_points = 0,
           decimal_offset = 0,
           token_offset = 0;
    while ('\0' != ch[0] && !is_atom_end_char(ch[0])) {
        num_digits += isdigit(ch[0]) ? 1 : 0;
        num_decimal_points += ch[0] == '.' ? 1 : 0;

        // Advance the offset until decimal point found
        decimal_offset = ch[0] == '.' ? token_offset : decimal_offset;
        strcat(token, ch);
        pos++;
        token_offset++;
        ch[0] = file_buffer[pos];
    }

    bool is_decimal = num_digits > 1
        && num_digits == strlen(token) - 1
        && num_decimal_points == 1;

    if (!is_decimal) {
        log_debug("parse_token_atom_decimal(): token %s not decimal, passing...\n", token);
        return LAR_PARSE_RESULT_PASS;
    }

    log_debug("Decimal found in token %s at pos %zu\n", token, decimal_offset);

    strncpy(whole, token, decimal_offset);
    whole[decimal_offset] = '\0';

    strcpy(frac, token + decimal_offset + 1); // Skip the decimal point

    node->node_type = LAR_NODE_ATOM_FIXED;
    node->atom.val_fixed = decimal_to_fixed(sign, whole, frac);
    (*buffer_pos) = pos;

    return LAR_PARSE_RESULT_OK;
}

LarParseResult parse_token_atom_string(const char* file_buffer,
    int* buffer_pos,
    LarNode* node)
{
    char token[LAR_PARSE_TOKEN_MAX] = "";
    char ch[2] = "";

    ch[0] = file_buffer[(*buffer_pos)];
    assert(ch[0] != '\0');

    if (ch[0] != '"') {
        return LAR_PARSE_RESULT_PASS;
    }

    (*buffer_pos)++;
    ch[0] = file_buffer[(*buffer_pos)];
    while (true) {
        if (ch[0] == '\0') {
            return LAR_PARSE_RESULT_ERROR;
        }

        if (ch[0] == '"') {
            (*buffer_pos)++;
            break;
        }

        if (ch[0] == '\\') {
            (*buffer_pos)++;
            ch[0] = file_buffer[(*buffer_pos)];
            if (ch[0] != '"' && ch[0] != '\\') {
                log_error("Backslash must be followed by \" or \\. Instead found: %s\n", ch);
                return LAR_PARSE_RESULT_ERROR;
            }
        }
        strcat(token, ch);
        (*buffer_pos)++;
        ch[0] = file_buffer[(*buffer_pos)];
    }

    log_debug("String found, adding to node: \"%s\"\n", token);
    node->node_type = LAR_NODE_ATOM_STRING;
    node->atom.val_string = malloc(strlen(token) + 1);
    strcpy(node->atom.val_string, token);
    return LAR_PARSE_RESULT_OK;
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

LarParseResult parse_token_atom_symbol(
    const char* file_buffer,
    int* buffer_pos,
    LarNode* node)
{
    char token[LAR_PARSE_TOKEN_MAX] = "";
    char ch[2] = "";
    int pos = *buffer_pos;

    ch[0] = file_buffer[pos];

    if (!is_valid_symbol_start(ch[0])) {
        return LAR_PARSE_RESULT_PASS;
    }

    while ('\0' != ch[0] && !is_atom_end_char(ch[0])) {
        if (!is_valid_symbol(ch[0])) {
            log_error("parse_token_atom_symbol: Invalid char: %c\n", ch[0]);
            return LAR_PARSE_RESULT_ERROR;
        }
        strcat(token, ch);
        pos++;
        ch[0] = file_buffer[pos];
    }

    log_debug("Symbol found, adding to node: %s\n", token);
    *buffer_pos = pos;
    node->node_type = LAR_NODE_ATOM_SYMBOL;
    node->atom.val_symbol = malloc(strlen(token) + 1);
    strcpy(node->atom.val_symbol, token);

    return LAR_PARSE_RESULT_OK;
}

LarParseResult parse_token_atom(
    const char* file_buffer,
    int* buffer_pos,
    LarNode* node)
{
    LarParseResult result;

    result = parse_token_atom_string(file_buffer, buffer_pos, node);
    if (result != LAR_PARSE_RESULT_PASS) {
        log_debug("parse_token_atom: found atom string\n");
        return result;
    }

    result = parse_token_atom_boolean(file_buffer, buffer_pos, node);
    if (result != LAR_PARSE_RESULT_PASS) {
        log_debug("parse_token_atom: found atom boolean\n");
        return result;
    }

    result = parse_token_atom_integer(file_buffer, buffer_pos, node);
    if (result != LAR_PARSE_RESULT_PASS) {
        return result;
    }

    result = parse_token_atom_fixed(file_buffer, buffer_pos, node);
    if (result != LAR_PARSE_RESULT_PASS) {
        return result;
    }

    result = parse_token_atom_symbol(file_buffer, buffer_pos, node);
    if (result != LAR_PARSE_RESULT_PASS) {
        log_debug("parse_token_atom: found atom symbol\n");
        return result;
    }

    log_error("parse_token_atom: atom not identifed at pos %d\n", *buffer_pos);
    return LAR_PARSE_RESULT_ERROR;
}
