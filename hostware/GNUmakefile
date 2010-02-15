CFLAGS ?=

CFLAGS += -std=c99
CFLAGS += -pedantic
CFLAGS += -Wall -Wextra
CFLAGS += -Werror

.PHONY: all
all: querySerial

.PHONY: ALL
ALL: all

CLEANFILES += querySerial

.PHONY: clean
clean:
	rm -f $(CLEANFILES)
