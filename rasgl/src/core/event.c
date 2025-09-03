#include "rasgl/core/event.h"
#include "rasgl/core/debug.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

RasEventSummaryItem event_summary_items[RAS_EVENT_SUMMARY_ITEMS_MAX];
size_t event_summary_items_count = 0;

static const char* g_repr_event_type[] = {
    "NONE",
    "SC_OBJ_CHANGE",
    "SC_OBJ_XFORM",
    "SC_OBJ_MOVE",
    "SG_SUMMARY",
    "RS_HLINES",
    "CM_CHANGE",
    "INVALID_MATH",
    "BUFFER_OVERFLOW",
    "TEST_FIXTURE"
};

void core_event_summary_init()
{
    event_summary_items_count = 0;
    for (int i = 0; i < RAS_EVENT_COUNT; i++) {
        snprintf(
            event_summary_items[i].name, RAS_EVENT_SUMMARY_NAME_MAX, "%s", g_repr_event_type[i]);
        event_summary_items[i].occurrences = 0;
        event_summary_items[i].last_message[0] = '\0';
        event_summary_items_count++;
    }

    log_add_callback(core_event_summary_cb, NULL, LOG_INFO);
}

char* core_repr_event_summary(char* buffer, size_t count, RasEventSummaryItem* event)
{
    if (!buffer || !event)
        return NULL;

    snprintf(buffer, count,
        "%-16s %06u %s",
        event->name,
        event->occurrences,
        event->last_message);

    return buffer;
}

void core_event_summary_update(
    RasEventType event_type,
    const char* message)
{

    if (event_type == 0 || event_type >= RAS_EVENT_COUNT) {
        return;
    }

    if (event_summary_items_count >= RAS_EVENT_SUMMARY_ITEMS_MAX)
        return;

    RasEventSummaryItem* event = &event_summary_items[event_type];
    event->occurrences++;
    snprintf(
        event->last_message,
        RAS_EVENT_SUMMARY_MESSAGE_MAX,
        "%s",
        message);
}

void core_event_summary_update2()
{
    if (event_summary_items_count >= RAS_EVENT_SUMMARY_ITEMS_MAX)
        return;
}

void core_event_summary_cb(log_Event* ev)
{
    if (ev->category == 0 || ev->category >= RAS_EVENT_COUNT) {
        return;
    }

    if (event_summary_items_count >= RAS_EVENT_SUMMARY_ITEMS_MAX)
        return;

    RasEventSummaryItem* event = &event_summary_items[ev->category];
    event_summary_items_count++;
    event->occurrences++;
    char buffer[RAS_EVENT_SUMMARY_MESSAGE_MAX];
    vsnprintf(
        buffer,
        RAS_EVENT_SUMMARY_MESSAGE_MAX,
        ev->fmt,
        ev->ap);

    core_event_summary_update(ev->category, buffer);
}

void ras_log_summary_flush()
{
    char summary[1024 * 5] = "";

    for (size_t i = 0; i < event_summary_items_count - 1; i++) {
        char buffer[512];
        if (event_summary_items[i].occurrences == 0)
            continue;
        core_repr_event_summary(buffer, sizeof(buffer), &event_summary_items[i]);
        strncat(summary, buffer, sizeof(summary) - strlen(summary) - 1);
        strncat(summary, "\n", sizeof(summary) - strlen(summary) - 1);
    }
    ras_log_info("Event summary:\n%s", summary);
}
