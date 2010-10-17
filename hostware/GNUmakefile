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
	rm -f *.d
	rm -f *.o


freemcan-signals.o : CFLAGS += -D_GNU_SOURCE
freemcan-device.o : CFLAGS += -D_GNU_SOURCE
freemcan-tui.o : CFLAGS += -D_GNU_SOURCE
freemcan-tui-main-select.o : CFLAGS += -D_GNU_SOURCE

TUI_COMMON_OBJ =
TUI_COMMON_OBJ += freemcan-checksum.o
TUI_COMMON_OBJ += freemcan-device.o
TUI_COMMON_OBJ += freemcan-export.o
TUI_COMMON_OBJ += frame.o
TUI_COMMON_OBJ += frame-parser.o
TUI_COMMON_OBJ += freemcan-iohelpers.o
TUI_COMMON_OBJ += freemcan-log.o
TUI_COMMON_OBJ += freemcan-packet.o
TUI_COMMON_OBJ += packet-parser.o
TUI_COMMON_OBJ += freemcan-signals.o
TUI_COMMON_OBJ += freemcan-tui.o
TUI_COMMON_OBJ += serial-setup.o

freemcan-tui : freemcan-tui-main-select.o $(TUI_COMMON_OBJ)
	$(LINK.c) $^ $(LDLIBS) -o $@

test-log : test-log.o freemcan-log.o
	$(LINK.c) $^ $(LDLIBS) -o $@


########################################################################
# Automatic dependency generation

%.d: %.c
	@set -e; rm -f $@; \
		$(COMPILE.c) -MM $< > $@.$$$$; \
		sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

ALL_CSOURCES = $(wildcard *.c)
include $(ALL_CSOURCES:.c=.d)


########################################################################
# End of GNUmakefile
