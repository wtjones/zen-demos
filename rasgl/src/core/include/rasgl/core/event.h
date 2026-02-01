#ifndef CORE_EVENT_H
#define CORE_EVENT_H

#include <stddef.h>
#include <stdint.h>

#define RAS_EVENT_SUMMARY_MESSAGE_MAX 255
#define RAS_EVENT_SUMMARY_NAME_MAX 16
#define RAS_EVENT_SUMMARY_ITEMS_MAX 16

typedef enum {
    RAS_EVENT_NONE = 0,
    RAS_EVENT_SC_OBJ_CHANGE,
    RAS_EVENT_SC_OBJ_XFORM,
    RAS_EVENT_SC_OBJ_MOVE,
    RAS_EVENT_SG_SUMMARY,
    RAS_EVENT_RS_OOB,
    RAS_EVENT_RS_HLINES,
    RAS_EVENT_RS_TRI_HLINES,
    RAS_EVENT_CM_CHANGE,
    RAS_EVENT_INVALID_MATH,
    RAS_EVENT_BUFFER_OVERFLOW,
    RAS_EVENT_TEST_FIXTURE,
    RAS_EVENT_COUNT
} RasEventType;

typedef struct {
    char name[RAS_EVENT_SUMMARY_NAME_MAX];
    uint32_t occurrences;
    char last_message[RAS_EVENT_SUMMARY_MESSAGE_MAX];
} RasEventSummaryItem;

void core_event_summary_init();

/**
 * @brief Update the event summary:
 * - Increment occurrence count
 * - Store last message
 *
 * @param event_type
 * @param message
 */
void core_event_summary_update(
    RasEventType event_type,
    const char* message);

char* core_repr_event_summary(char* buffer, size_t count, RasEventSummaryItem* event);

#endif
