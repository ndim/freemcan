bin_PROGRAMS ?=

CFLAGS ?=
LDFLAGS ?=
LDLIBS ?=

CFLAGS += -std=c99
CFLAGS += -pedantic
CFLAGS += -Wall -Wextra
CFLAGS += -Werror

bin_PROGRAMS += querySerial
CLEANFILES   += querySerial

CLEANFILES += querySerial.o
CLEANFILES += serial-setup.o

.PHONY: all
all: $(bin_PROGRAMS)

.PHONY: ALL
ALL: all

.PHONY: clean
clean:
	rm -f $(CLEANFILES)

querySerial : querySerial.o serial-setup.o
	$(LINK.c) $^ $(LDLIBS) -o $@

querySerial.o  : querySerial.c  serial-setup.h
serial-setup.o : serial-setup.c serial-setup.h
