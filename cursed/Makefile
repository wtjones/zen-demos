PKGS    := ncursesw
CC      := gcc
CFLAGS  := -Wall -O2 -std=c99 $(shell pkg-config --cflags $(PKGS))
LDFLAGS := $(shell pkg-config --libs $(PKGS))
AS      := as
ASFLAGS := -gdbb --32
PROG    := demo
SRCS    = demo.c

.PHONY: all clean

all: $(PROG)

clean:
	$(RM) *.o $(PROGS)

$(PROG):$(SRCS)
	$(CC) $(CFLAGS) -o $(PROG) $(SRCS) $(LDFLAGS)
