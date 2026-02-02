#include "rasgl/hosted/event_log.h"
#include "log.c/src/log.h"
#include "rasgl/core/event.h"

extern size_t event_summary_items_count;
extern RasEventSummaryItem event_summary_items[RAS_EVENT_SUMMARY_ITEMS_MAX];

void hosted_event_summary_log_init()
{
    log_add_callback(hosted_event_summary_cb, NULL, LOG_INFO);
}

void hosted_event_summary_cb(log_Event* ev)
{
    if (ev->category == 0 || ev->category >= RAS_EVENT_COUNT) {
        return;
    }

    if (event_summary_items_count >= RAS_EVENT_SUMMARY_ITEMS_MAX)
        return;

    RasEventSummaryItem* event = &event_summary_items[ev->category];
    event_summary_items_count++;
    event->occurrences++;
    static char buffer[RAS_EVENT_SUMMARY_MESSAGE_MAX];
    vsnprintf(
        buffer,
        RAS_EVENT_SUMMARY_MESSAGE_MAX,
        ev->fmt,
        ev->ap);

    core_event_summary_update(ev->category, buffer);
}
