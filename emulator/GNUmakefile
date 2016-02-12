# Example command line:
# $ killall erl_unix_port freemcan-text; make && rm -f testdev && erl -noshell -s freemcan_emulator start testdev -s init stop

priv_PROGRAMS ?=

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

.PHONY: all
all: all-local

include ../common.mk

-include local.mk

.PHONY: ALL
ALL: all

ifeq ($(HAVE_ERL),no)

.PHONY: all-local
all-local:

else

priv_PROGRAMS += priv/erl_unix_port
priv/erl_unix_port : .objs/erl_unix_port.o
	@$(MKDIR_P) $(@D)
	$(LINK.c) -o $@ $(LDLIBS) $^


TARGETS += ebin/fmemu.app
ebin/fmemu.app: src/fmemu.app.src
	@$(MKDIR_P) $(@D)
	cat $< > $@

ebin/%.beam: src/%.erl
	@$(MKDIR_P) $(@D)
	$(ERLC) -o $(@D) $(ERLC_FLAGS) $<

.objs/%.o: c_src/%.c
	@$(MKDIR_P) $(@D)
	$(COMPILE.c) -o $@ -c $<

.deps/%.c.dep: c_src/%.c
	@$(MKDIR_P) $(@D)
	$(COMPILE.c) -MM -MT ".objs/$*.o $@ " $< > $@

.deps/%.erl.dep: src/%.erl
	@$(MKDIR_P) $(@D)
	$(ERLC) -o ebin $(ERLC_FLAGS) -M -MT "ebin/$*.beam $@ " $< > $@

include $(foreach F, $(notdir $(wildcard c_src/*.c src/*.erl)), .deps/$(F).dep)

.PHONY: all-local
all-local: $(priv_PROGRAMS) $(BEAMS) $(TARGETS)

endif

.PHONY: clean
clean: clean-common
	rm -rf ebin
	rm -rf priv
	rm -rf .deps
	rm -rf .objs
