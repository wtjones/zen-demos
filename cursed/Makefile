PKGS    := ncursesw
CC      := gcc
CFLAGS  := -g -Wall -O0 -std=c99 $(shell pkg-config --cflags $(PKGS))
LDFLAGS := $(shell pkg-config --libs $(PKGS))
AS      := as
ASFLAGS := -gdbb --32
PROG    := cursed
SRCS    = 3rdparty/log.c/src/log.c main.c world.c game.c draw.c input.c

.PHONY: all clean

all: $(PROG)

clean:
	$(RM) *.o $(PROGS)

$(PROG):$(SRCS)
	$(CC) $(CFLAGS) -o $(PROG) $(SRCS) $(LDFLAGS)
