# Example command line:
# $ killall erl_unix_port freemcan-text; make && rm -f testdev && erl -noshell -s freemcan_emulator start testdev -s init stop

bin_PROGRAMS ?=

ERLC = erlc
ERLC_FLAGS ?=
ERLC_FLAGS += +debug

CFLAGS ?=
LDFLAGS ?=
LDLIBS ?=

CFLAGS += -std=c99
CFLAGS += -pedantic
CFLAGS += -Wall -Wextra
CFLAGS += -Werror
CFLAGS += -g
CFLAGS += -I../include

ERL_SOURCES = freemcan_emulator.erl histogram_emulator.erl
BEAMS = $(ERL_SOURCES:.erl=.beam)
CLEANFILES += $(BEAMS)

ERL_LIB = /usr/lib/erlang/lib
ERL_INTERFACE_ROOT = $(firstword $(wildcard $(ERL_LIB)/erl_interface-*))

bin_PROGRAMS += erl_unix_port
CLEANFILES   += erl_unix_port
CLEANFILES   += erl_unix_port.o

.PHONY: all
all: $(bin_PROGRAMS) $(BEAMS)

.PHONY: ALL
ALL: all

.PHONY: clean
clean:
	rm -f $(bin_PROGRAMS)
	rm -f $(BEAMS)
	rm -f $(CLEANFILES)

erl_unix_port : erl_unix_port.o
erl_unix_port : LDLIBS += -L$(ERL_INTERFACE_ROOT)/lib -lei
erl_unix_port.o : CFLAGS += -I$(ERL_INTERFACE_ROOT)/include
erl_unix_port.o : erl_unix_port.c

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $<
