# FIXME: Add proper dependencies for all those *.c and *.h files.

bin_PROGRAMS ?=

CFLAGS ?=
LDFLAGS ?=
LDLIBS ?=

CFLAGS += -std=c99
CFLAGS += -pedantic
CFLAGS += -Wall -Wextra
CFLAGS += -Werror
CFLAGS += -g
CFLAGS += -I../include
CFLAGS += -Wp,-D_FORTIFY_SOURCE=2 -fexceptions -fstack-protector --param=ssp-buffer-size=4

bin_PROGRAMS += querySerial
CLEANFILES   += querySerial

CLEANFILES += querySerial.o
CLEANFILES += serial-setup.o

bin_PROGRAMS += freemcan-text
CLEANFILES   += freemcan-text
CLEANFILES   += freemcan-text.o

bin_PROGRAMS += test-log
CLEANFILES   += test-log

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

freemcan-common.o : CFLAGS += -D_GNU_SOURCE
freemcan-common.o : freemcan-common.c freemcan-common.h freemcan-log.h

freemcan-device.o : CFLAGS += -D_GNU_SOURCE
freemcan-device.o : freemcan-device.c freemcan-device.h freemcan-frame.h freemcan-log.h freemcan-select.h serial-setup.h

freemcan-frame.o : freemcan-frame.c freemcan-frame.h freemcan-log.h ../include/frame-defs.h

freemcan-log.o : freemcan-log.c freemcan-log.h

freemcan-packet.o : freemcan-packet.c freemcan-packet.h freemcan-frame.h freemcan-log.h ../include/frame-defs.h ../include/packet-defs.h

freemcan-select.o : freemcan-select.c freemcan-select.h freemcan-log.h

freemcan-text.o : CFLAGS += -D_GNU_SOURCE
freemcan-text.o : freemcan-text.c freemcan-common.h freemcan-device.h freemcan-frame.h freemcan-log.h freemcan-select.h

HOST_SRC = common device frame log packet select text
HOST_OBJ = $(HOST_SRC:%=freemcan-%.o)

freemcan-text : freemcan-text.o serial-setup.o $(HOST_OBJ)
	$(LINK.c) $^ $(LDLIBS) -o $@


test-log : test-log.o freemcan-log.o
	$(LINK.c) $^ $(LDLIBS) -o $@


depend-here:
	if grep '^# DO NOT DELETE' $(MAKEFILE) >/dev/null; \
	then \
		sed -e '/^# DO NOT DELETE/,$$d' $(MAKEFILE) > \
			$(MAKEFILE).$$$$ && \
		$(MV) $(MAKEFILE).$$$$ $(MAKEFILE); \
	fi
	echo '# DO NOT DELETE THIS LINE -- make depend depends on it.' \
		>> $(MAKEFILE); \
	$(CC) -M -mmcu=$(MCU) $(CDEFS) $(CINCS) $(SRC) $(ASRC) >> $(MAKEFILE)
