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


.PHONY: all
all: all-local

include ../common.mk

.PHONY: ALL
ALL: all

ifeq ($(HAVE_ERL),no)

.PHONY: all-local
all-local:

else

bin_PROGRAMS += priv/erl_unix_port

ERL_INTERFACE_BASENAME := erl_interface-*

ERL_LIB_CANDIDATES =
ERL_LIB_CANDIDATES += /usr/lib64/erlang/lib/$(ERL_INTERFACE_BASENAME)
ERL_LIB_CANDIDATES += /usr/lib/erlang/lib/$(ERL_INTERFACE_BASENAME)

ERL_INTERFACE_ROOT = $(firstword $(wildcard $(ERL_LIB_CANDIDATES)))
ifeq (,$(ERL_INTERFACE_ROOT))
$(error Could not find erl_interface)
endif

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

.PHONY: all-local
all-local: $(priv_PROGRAMS) $(BEAMS) $(TARGETS)

endif

.PHONY: clean
clean: clean-common
	rm -f $(bin_PROGRAMS)
	rm -f $(BEAMS)
	rm -f $(CLEANFILES)
