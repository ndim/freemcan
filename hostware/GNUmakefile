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

bin_PROGRAMS += querySerial
CLEANFILES   += querySerial

CLEANFILES += querySerial.o
CLEANFILES += serial-setup.o

bin_PROGRAMS += erl_unix_port
CLEANFILES   += erl_unix_port
CLEANFILES   += erl_unix_port.o

bin_PROGRAMS += freemcan
CLEANFILES   += freemcan
CLEANFILES   += freemcan-main.o

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

freemcan-main.o : CFLAGS += -D_GNU_SOURCE

freemcan : freemcan-main.o
	$(LINK.c) $^ $(LDLIBS) -o $@

ERL_LIB = /usr/lib/erlang/lib
ERL_INTERFACE_ROOT = $(firstword $(wildcard $(ERL_LIB)/erl_interface-*))

erl_unix_port : erl_unix_port.o
erl_unix_port : LDLIBS += -L$(ERL_INTERFACE_ROOT)/lib -lei
erl_unix_port.o : CFLAGS += -I$(ERL_INTERFACE_ROOT)/include
erl_unix_port.o : erl_unix_port.c

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $<
