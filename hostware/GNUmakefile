bin_PROGRAMS ?=

CFLAGS ?=
LDFLAGS ?=
LDLIBS ?=

ERLC = erlc
ERLC_FLAGS =
ERLC_FLAGS += +debug

CFLAGS += -std=c99
CFLAGS += -pedantic
CFLAGS += -Wall -Wextra
CFLAGS += -Werror
CFLAGS += -I../include

bin_PROGRAMS += querySerial
CLEANFILES   += querySerial

CLEANFILES += querySerial.o
CLEANFILES += serial-setup.o

bin_PROGRAMS += erl_unix_port
CLEANFILES   += erl_unix_port
CLEANFILES   += erl_unix_port.o

# bin_PROGRAMS += freemcan-ncurses
# CLEANFILES   += freemcan-ncurses
# CLEANFILES   += freemcan-ncurses.o

bin_PROGRAMS += freemcan-text
CLEANFILES   += freemcan-text
CLEANFILES   += freemcan-text.o

ERL_SOURCES = freemcan_emulator.erl
BEAMS = $(ERL_SOURCES:.erl=.beam)
CLEANFILES += $(BEAMS)

.PHONY: all
all: $(bin_PROGRAMS) $(BEAMS)

.PHONY: ALL
ALL: all

.PHONY: clean
clean:
	rm -f $(CLEANFILES)

querySerial : querySerial.o serial-setup.o
	$(LINK.c) $^ $(LDLIBS) -o $@

querySerial.o  : querySerial.c  serial-setup.h
serial-setup.o : serial-setup.c serial-setup.h

freemcan-ncurses.o : CFLAGS += -D_GNU_SOURCE
freemcan-ncurses.o : freemcan-main.c serial-setup.h

freemcan-text.o : CFLAGS += -D_GNU_SOURCE
freemcan-text.o : freemcan-text.c freemcan.h freemcan-common.h freemcan-device.h freemcan-frame.h freemcan-log.h freemcan-select.h

freemcan-log.o : freemcan-log.c freemcan-log.h

freemcan-common.o : CFLAGS += -D_GNU_SOURCE
freemcan-common.o : freemcan-common.c freemcan-common.h

freemcan-select.o : freemcan-select.c freemcan-select.h freemcan-log.h

freemcan-device.o : CFLAGS += -D_GNU_SOURCE
freemcan-device.o : freemcan-device.c freemcan-device.h freemcan-log.h freemcan-select.h serial-setup.h

HOST_SRC = common device log select text
HOST_OBJ = $(HOST_SRC:%=freemcan-%.o)

freemcan-ncurses : LDLIBS += -lncurses
freemcan-ncurses : freemcan-ncurses.o serial-setup.o $(HOST_OBJ)
	$(LINK.c) $^ $(LDLIBS) -o $@

freemcan-text : freemcan-text.o serial-setup.o $(HOST_OBJ)
	$(LINK.c) $^ $(LDLIBS) -o $@

ERL_LIB = /usr/lib/erlang/lib
ERL_INTERFACE_ROOT = $(firstword $(wildcard $(ERL_LIB)/erl_interface-*))

erl_unix_port : erl_unix_port.o
erl_unix_port : LDLIBS += -L$(ERL_INTERFACE_ROOT)/lib -lei
erl_unix_port.o : CFLAGS += -I$(ERL_INTERFACE_ROOT)/include
erl_unix_port.o : erl_unix_port.c

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $<
