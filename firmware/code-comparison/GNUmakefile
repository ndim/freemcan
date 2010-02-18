ARCHES = avr msp430 arm-gp2x-linux i686-pc-mingw32 NATIVE

LSS_TARGETS = $(foreach a,$(ARCHES),histogram.$(a).lss)
LST_TARGETS = $(foreach a,$(ARCHES),histogram.$(a).lst)
OBJ_TARGETS = $(foreach a,$(ARCHES),histogram.$(a).o)

TARGETS = $(LSS_TARGETS) $(LST_TARGETS) $(OBJ_TARGETS)

.PHONY: all
all: $(TARGETS)

.PHONY: clean
clean:
	rm -f *.o *.lss *.lst

CFLAGS =
CFLAGS += -std=c99
CFLAGS += -pedantic
CFLAGS += -g
CFLAGS += -Wa,-adhlns=$(@:.o=.lst),-gstabs
# CFLAGS += -O
CFLAGS += -Wall -Werror
# CFLAGS += -Wextra

histogram.%.lss: histogram.%.o
	$(if $(subst NATIVE,,$*),$*-,)objdump -h -S $< > $@

histogram.%.o histogram.%.lst: histogram.c histogram.h
	$(if $(subst NATIVE,,$*),$*-,)gcc $(CFLAGS) -c $< -o $@

