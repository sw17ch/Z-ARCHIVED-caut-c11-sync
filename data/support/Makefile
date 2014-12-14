CC=clang
CFLAGS=-O3 -Wall -Wextra -std=c11 -pedantic
PROG=run_tests

CFILES := $(wildcard *.c)
HFILES := $(wildcard *.h)

all: $(PROG)
clean:
	rm $(PROG)

$(PROG): $(HFILES) $(CFILES)
	$(CC) $(CFLAGS) $(CFILES) -o $@
