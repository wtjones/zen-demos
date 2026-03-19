#include "usage.h"
#include "rasgl/core/debug.h"
// clang-format off
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <malloc.h>
// clang-format on

// Linker-provided section symbols (from cmake-psx/executable.ld)
extern char _textStart[];
extern char _textEnd[];
extern char _dataStart[];
extern char _dataEnd[];
extern char _bssStart[];
extern char _bssEnd[];

/**
 * @brief Print linker section sizes, heap and approximate stack info for PSX.
 *
 */
void psx_log_usage_info(void)
{
    uintptr_t text_sz = (uintptr_t)_textEnd - (uintptr_t)_textStart;
    uintptr_t data_sz = (uintptr_t)_dataEnd - (uintptr_t)_dataStart;
    uintptr_t bss_sz = (uintptr_t)_bssEnd - (uintptr_t)_bssStart;
    uintptr_t static_total = text_sz + data_sz + bss_sz;

    ras_log_info("Static sizes: text=%u data=%u bss=%u total=%u",
        (unsigned)text_sz, (unsigned)data_sz, (unsigned)bss_sz, (unsigned)static_total);

    ras_log_info("PSX heap usage (malloc): %u bytes used, %u free",
        (unsigned)mallinfo2().uordblks, (unsigned)mallinfo2().fordblks);

    // Approximate stack pointer by taking address of a local variable.
    {
        int stack_marker = 0;
        ras_log_info("Approx stack address: %p", (void*)&stack_marker);
    }

    ras_log_info("PSX heap usage: %d bytes. Free: %d bytes\n",
        mallinfo2().uordblks, mallinfo2().fordblks);
}

void psx_log_heap(void)
{
    ras_log_info("PSX heap usage: %u bytes. Free: %u bytes\n",
        (unsigned)mallinfo2().uordblks, (unsigned)mallinfo2().fordblks);
}
