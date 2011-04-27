bin_PROGRAMS ?=

# Add call possible -I flags to ALL_CFLAGS later for include file
# dependeny detection
ALL_CFLAGS =

CFLAGS ?=
LDFLAGS ?=
LDLIBS ?=

CFLAGS += -std=c99
CFLAGS += -pedantic
CFLAGS += -Wall -Wextra
# CFLAGS += -Werror
CFLAGS += -g
CFLAGS += -I../include
CFLAGS += -Wp,-D_FORTIFY_SOURCE=2 -fexceptions -fstack-protector --param=ssp-buffer-size=4
LDLIBS += -lm


include ../common.mk

bin_PROGRAMS += freemcan-tui
CLEANFILES   += freemcan-tui

bin_PROGRAMS += freemcan-tui
CLEANFILES   += freemcan-tui

bin_PROGRAMS += test-log
CLEANFILES   += test-log

# Add to or override some variables here, if you want to
-include local.mk

.PHONY: all
all: $(bin_PROGRAMS)

# Legacy target
.PHONY: ALL
ALL: all

.PHONY: clean
clean:
	rm -f $(CLEANFILES)
	rm -f *~
	rm -rf .deps .objs
	rm -f *.o


.objs/freemcan-signals.o : CFLAGS += -D_GNU_SOURCE
.objs/freemcan-device.o : CFLAGS += -D_GNU_SOURCE
.objs/freemcan-tui.o : CFLAGS += -D_GNU_SOURCE
.objs/freemcan-tui-main-select.o : CFLAGS += -D_GNU_SOURCE

TUI_COMMON_OBJ =
TUI_COMMON_OBJ += .objs/freemcan-checksum.o
TUI_COMMON_OBJ += .objs/freemcan-device.o
TUI_COMMON_OBJ += .objs/freemcan-export.o
TUI_COMMON_OBJ += .objs/frame.o
TUI_COMMON_OBJ += .objs/frame-parser.o
TUI_COMMON_OBJ += .objs/freemcan-iohelpers.o
TUI_COMMON_OBJ += .objs/freemcan-log.o
TUI_COMMON_OBJ += .objs/freemcan-packet.o
TUI_COMMON_OBJ += .objs/packet-value-table.o
TUI_COMMON_OBJ += .objs/personality-info.o
TUI_COMMON_OBJ += .objs/packet-parser.o
TUI_COMMON_OBJ += .objs/freemcan-signals.o
TUI_COMMON_OBJ += .objs/freemcan-tui.o
TUI_COMMON_OBJ += .objs/serial-setup.o

freemcan-tui : .objs/freemcan-tui-main-select.o $(TUI_COMMON_OBJ)
	$(LINK.c) $^ $(LDLIBS) -o $@

test-log : .objs/test-log.o .objs/freemcan-log.o
	$(LINK.c) $^ $(LDLIBS) -o $@

.objs/%.o: %.c
	@$(MKDIR_P) $(@D)
	$(COMPILE.c) -o $@ $<

########################################################################
# Automatic dependency generation

.deps/%.c.dep: %.c
	@set -e; rm -f $@; \
		$(MKDIR_P) $(@D); \
		$(COMPILE.c) -MM $< > $@.$$$$; \
		sed 's,\($*\)\.o[ :]*,.objs/\1.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

include $(foreach F, $(wildcard *.c), .deps/$(F).dep)


########################################################################
# End of GNUmakefile
