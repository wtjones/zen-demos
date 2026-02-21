
#pragma once

struct mallinfo2 {
    size_t arena;    /* Non-mmapped space allocated from system */
    size_t ordblks;  /* Number of free chunks */
    size_t smblks;   /* Number of fastbin blocks */
    size_t hblks;    /* Number of mmapped regions */
    size_t hblkhd;   /* Space in mmapped regions */
    size_t usmblks;  /* Maximum total allocated space */
    size_t fsmblks;  /* Space in freed fastbin blocks */
    size_t uordblks; /* Total allocated space */
    size_t fordblks; /* Total free space */
    size_t keepcost; /* Top-most, releasable (via malloc_trim) space */
};

struct mallinfo2 mallinfo2(void);
