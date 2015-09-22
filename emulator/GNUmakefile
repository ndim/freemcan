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

TARGETS =

ERL_BASES =
ERL_BASES += histogram_emulator
ERL_BASES += fmemu_app
ERL_BASES += fmemu_sup
ERL_BASES += fmemu_frame_fsm
ERL_BASES += fmemu_byte_fsm
ERL_BASES += fmemu_unix_port
ERL_BASES += fmemu_util

BEAMS       = $(foreach b,$(ERL_BASES),ebin/$(b).beam)
ERL_SOURCES = $(foreach b,$(ERL_BASES),src/$(b).erl)
CLEANFILES += $(BEAMS)

ERL_LIB = $(shell if test -d /usr/lib64/erlang/lib; then echo /usr/lib64/erlang/lib; else echo /usr/lib/erlang/lib; fi)
ERL_INTERFACE_ROOT = $(firstword $(wildcard $(ERL_LIB)/erl_interface-*))

.PHONY: all
all: all-internal

.PHONY: ALL
ALL: all

bin_PROGRAMS += priv/erl_unix_port
CLEANFILES   += priv/erl_unix_port
priv/erl_unix_port : LDLIBS += -L$(ERL_INTERFACE_ROOT)/lib -lei
priv/erl_unix_port : c_src/erl_unix_port.o
	$(LINK.c) -o $@ $(LDLIBS) $^

CLEANFILES   += c_src/erl_unix_port.o
c_src/erl_unix_port.o : CFLAGS += -I$(ERL_INTERFACE_ROOT)/include
c_src/erl_unix_port.o : c_src/erl_unix_port.c

ebin/%.beam: src/%.erl
	$(ERLC) -o $(@D) $(ERLC_FLAGS) $<

CLEANFILES += ebin/fmemu.app
TARGETS += ebin/fmemu.app
ebin/fmemu.app: src/fmemu.app.src
	cat $< > $@

.PHONY: all-internal
all-internal: $(bin_PROGRAMS) $(BEAMS) $(TARGETS)

.PHONY: clean
clean:
	rm -f $(bin_PROGRAMS)
	rm -f $(BEAMS)
	rm -f $(CLEANFILES)
