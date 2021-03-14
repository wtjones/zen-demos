PKGS    := ncursesw
CC      := gcc
CFLAGS  := -Wall -O2 -std=c99 $(shell pkg-config --cflags $(PKGS))
LDFLAGS := $(shell pkg-config --libs $(PKGS))
AS      := as
ASFLAGS := -gdbb --32
PROG    := cursed
SRCS    = main.c draw.c input.c game.c

.PHONY: all clean

all: $(PROG)

clean:
	$(RM) *.o $(PROGS)

$(PROG):$(SRCS)
	$(CC) $(CFLAGS) -o $(PROG) $(SRCS) $(LDFLAGS)
