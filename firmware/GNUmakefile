# Hey Emacs, this is a -*- makefile -*-

# Notes:
#   * A default firmware is between 1700 and 1800 bytes.
#   * Run "make BUILD_FREEMCAN_INVENTED_HISTOGRAM=yes" to build the
#     histogram inventor into the firmware
#     CAUTION: This adds about 3200 byte to the firmware.
#   * Run "make BUILD_FREEMCAN_PRINTF=yes" to build the printf code
#     into the firmware.
#     CAUTION: This adds about 1500 byte to the firmware.
#   * You can define these variables in settings.mk if you like.
#   * "make program" takes about 8 seconds with a 6600 byte "fat"
#     firmware image on a proper RS232 port.

# AVR-GCC Makefile template, derived from the WinAVR template (which
# is public domain), believed to be neutral to any flavor of "make"
# (GNU make, BSD make, SysV make).
#
# Relying on GNU make features now as I can't stand the compatible
# make deficiencies.


CFLAGS =
CLEANFILES =
COMMON_OBJ =
TARGETS =

include ../common.mk

# Override settings by defining them in settings.mk Things not defined
# in settings.mk will be later defined with ?= later in this Makefile.
# AVRDUDE_PROGRAMMER, AVRDUDE_PORT, MCU are good candidates for
# settings.mk.
-include settings.mk


MCU ?= atmega644p
FORMAT = ihex
TARGETS += adc-ext-mca
TARGETS += adc-int-mca
CFLAGS += -Os

# BUG: Needs proper hooking up with new unified-firmware build system
ifeq ($(BUILD_FREEMCAN_INVENTED_HISTOGRAM),yes)
COMMON_OBJ += invented-histogram.o
CPPFLAGS += -DINVENTED_HISTOGRAM
endif

# BUG: Needs proper hooking up with new unified-firmware build system
ifeq ($(BUILD_FREEMCAN_PRINTF),yes)
COMMON_OBJ += uart-printf.o
CPPFLAGS += -DHAVE_UPRINTF_IMPLEMENTATION
endif

# Debugging format.
# Native formats for AVR-GCC's -g are stabs [default], or dwarf-2.
# AVR (extended) COFF requires stabs, plus an avr-objcopy run.
CFLAGS += -gstabs

# Compiler flag to set the C Standard level.
# c89   - "ANSI" C
# gnu89 - c89 plus GCC extensions
# c99   - ISO C99 standard (not yet fully implemented)
# gnu99 - c99 plus GCC extensions
CFLAGS += -std=gnu99


# Place -D or -U options here
CPPFLAGS +=

# Place -I options here
CPPFLAGS += -I../include


CPPFLAGS += -Wall -Wextra
CFLAGS += -Wstrict-prototypes
# Some more flags that might be useful for tuning:
# CFLAGS += -funsigned-char
# CFLAGS += -funsigned-bitfields
# CFLAGS += -fpack-struct
CFLAGS += -fshort-enums
# CFLAGS += -Wa,-adhlns=$(<:.c=.lst)


ASFLAGS += -Wa,-adhlns=$(<:.S=.lst),-gstabs


#Additional libraries.

# Minimalistic printf version
# LDLIBS += -Wl,-u,vfprintf -lprintf_min

# Floating point printf version (requires MATH_LIB = -lm below)
# LDLIBS += -Wl,-u,vfprintf -lprintf_flt

# Minimalistic scanf version
# LDLIBS += -Wl,-u,vfscanf -lscanf_min

# Floating point + %[ scanf version (requires MATH_LIB = -lm below)
# LDLIBS += -Wl,-u,vfscanf -lscanf_flt

# Math library
LDLIBS += -lm

# External memory options

# 64 KB of external RAM, starting after internal RAM (ATmega128!),
# used for variables (.data/.bss) and heap (malloc()).
# LDFLAGS += -Wl,--section-start,.data=0x801100,--defsym=__heap_end=0x80ffff

# 64 KB of external RAM, starting after internal RAM (ATmega128!),
# only used for heap (malloc()).
# LDFLAGS += -Wl,--defsym=__heap_start=0x801100,--defsym=__heap_end=0x80ffff

# Create map file on linking
LDFLAGS += -Wl,-Map=$(TARGET).map,--cref


# Programming support using avrdude. Settings and variables.

AVRDUDE_PROGRAMMER ?= ponyser
AVRDUDE_PORT ?= /dev/ttyS0

AVRDUDE_WRITE_FLASH = -U flash:w:firmware-$(TARGET).hex
#AVRDUDE_WRITE_EEPROM = -U eeprom:w:firmware-$(TARGET).eep


# Uncomment the following if you want avrdude's erase cycle counter.
# Note that this counter needs to be initialized first using -Yn,
# see avrdude manual.
#AVRDUDE_ERASE_COUNTER = -y

# Uncomment the following if you do /not/ wish a verification to be
# performed after programming the device.
#AVRDUDE_NO_VERIFY = -V

# Increase verbosity level.  Please use this when submitting bug
# reports about avrdude. See <http://savannah.nongnu.org/projects/avrdude>
# to submit bug reports.
#AVRDUDE_VERBOSE = -v -v

AVRDUDE_BASIC = -p $(MCU) -P $(AVRDUDE_PORT) -c $(AVRDUDE_PROGRAMMER)
AVRDUDE_FLAGS = $(AVRDUDE_BASIC) $(AVRDUDE_NO_VERIFY) $(AVRDUDE_VERBOSE) $(AVRDUDE_ERASE_COUNTER)


ERL = erl
CC = avr-gcc
OBJCOPY = avr-objcopy
OBJDUMP = avr-objdump
SIZE = avr-size
NM = avr-nm
AVRDUDE = avrdude
REMOVE = rm -f
MV = mv -f
OD = od

# Define the common object files.
# The object files specific to firmware personalities will not be listed here.
COMMON_OBJ += main.o
COMMON_OBJ += uart-comm.o
COMMON_OBJ += frame-comm.o
COMMON_OBJ += packet-comm.o
COMMON_OBJ += wdt-softreset.o
COMMON_OBJ += firmware-version.o

# Add target processor to flags.
CPPFLAGS += -mmcu=$(MCU)

CPPFLAGS += -I.
ASFLAGS += -x assembler-with-cpp

# Default target.
all: elf hex checks

ALL: elf hex lss sym fuses checks

elf: $(foreach TARGET, $(TARGETS), firmware-$(TARGET).elf)
hex: $(foreach TARGET, $(TARGETS), firmware-$(TARGET).hex)
eep: $(foreach TARGET, $(TARGETS), firmware-$(TARGET).eep)
lss: $(foreach TARGET, $(TARGETS), firmware-$(TARGET).lss)
sym: $(foreach TARGET, $(TARGETS), firmware-$(TARGET).sym)
fuses: $(foreach TARGET, $(TARGETS), firmware-$(TARGET).fuses)


# Program the device.
program-% : TARGET = $*
program-%: firmware-%.hex firmware-%.eep
	$(AVRDUDE) $(AVRDUDE_FLAGS) $(AVRDUDE_WRITE_FLASH) $(AVRDUDE_WRITE_EEPROM)


CLEANFILES += $(foreach TARGET, $(TARGETS), firmware-$(TARGET).fuses)
CLEANFILES += $(foreach TARGET, $(TARGETS), firmware-$(TARGET).lfuse.txt)
CLEANFILES += $(foreach TARGET, $(TARGETS), firmware-$(TARGET).hfuse.txt)
CLEANFILES += $(foreach TARGET, $(TARGETS), firmware-$(TARGET).efuse.txt)
firmware-%.fuses: firmware-%.elf
	$(OBJCOPY) -j .fuse -O binary $< $@
	@chmod -x $@
	@echo "Split $@ into $*.{l,h,e}fuse.txt"; \
	$(OD) -A n -t xC $@ | \
	{ read lfuse hfuse efuse restofline; \
	  echo "0x$$lfuse" > firmware-$*.lfuse.txt; \
	  echo "0x$$hfuse" > firmware-$*.hfuse.txt; \
	  echo "0x$$efuse" > firmware-$*.efuse.txt; \
	}

compare-fuses-%: read-fuses-% firmware-%.fuses
	@echo "New fuse values to be written to device:"; \
	for f in lfuse hfuse efuse; do \
		echo "    $$f=$$(cat "$(TARGET).$$f.txt")"; \
	done

program-fuses-%: firmware-%.fuses
	@echo "Writing fuses to device:"; \
	for f in lfuse hfuse efuse; do \
		echo "    $$f=$$(cat "firmware-$*.$$f.txt")"; \
	done
	$(AVRDUDE) $(AVRDUDE_FLAGS) -u \
		-U lfuse:w:$(shell cat firmware-$*.lfuse.txt):m \
		-U hfuse:w:$(shell cat firmware-$*.hfuse.txt):m \
		-U efuse:w:$(shell cat firmware-$*.efuse.txt):m

read-fuses-%:
	$(AVRDUDE) $(AVRDUDE_FLAGS) \
		-U lfuse:r:DEVICE-$*.lfuse.txt:h \
		-U hfuse:r:DEVICE-$*.hfuse.txt:h \
		-U efuse:r:DEVICE-$*.efuse.txt:h
	@echo "Read fuses from device:"; \
	for f in lfuse hfuse efuse; do \
		echo "    $$f=$$(cat "DEVICE-$*.$$f.txt")"; \
	done

.SUFFIXES: .elf .hex .eep .lss .sym

firmware-%.hex: firmware-%.elf
	$(OBJCOPY) -O $(FORMAT) -R .eeprom -R .fuse $< $@

firmware-%.eep: firmware-%.elf
	-$(OBJCOPY) -j .eeprom --set-section-flags=.eeprom="alloc,load" \
		--change-section-lma .eeprom=0 -O $(FORMAT) $< $@

# Create extended listing file from ELF output file.
firmware-%.lss: firmware-%.elf
	$(OBJDUMP) -h -S $< > $@

# Create a symbol table from ELF output file.
firmware-%.sym: firmware-%.elf
	$(NM) -n $< > $@



# Link: create ELF output file from object files.
firmware-%.elf : TARGET = $*
firmware-adc-ext-mca.elf : $(COMMON_OBJ) measurement-timer-ISR-countdown-and-stop.o adc-ext-mca.o adc-ext-histogram.o histogram.o
	$(CC) $(CFLAGS) $(CPPFLAGS) $(filter %.o, $^) $(LDLIBS) --output $@ $(LDFLAGS)
firmware-adc-int-mca.elf : $(COMMON_OBJ) measurement-timer-ISR-countdown-and-stop.o adc-int-mca.o adc-int-histogram.o histogram.o
	$(CC) $(CFLAGS) $(CPPFLAGS) $(filter %.o, $^) $(LDLIBS) --output $@ $(LDFLAGS)


# Check that the SRAM usage fits into the ATmega644's 4K SRAM
# size: __bss_end must be <= 0x00801100.
#
# The 0x00800000 is avr-gcc's Harvard architecture trick, and the
# first 0x100 bytes in the addr space are CPU and IO registers, after
# which the 4K of SRAM start, up to and including address 0x10ff.
CHECKS += bss-end
check-bss-end-%.check: firmware-%.elf
	@MAX_BSS_END="$$(printf "%d\n" 0x1100)"; \
	BSS_END="$$(nm $< | awk '/__bss_end/ { print (strtonum("0x"$$1) % 0x10000); }')"; \
	printf "CHECK($*): MAX_BSS_END=0x%x >= BSS_END=0x%x\n" "$$MAX_BSS_END" "$$BSS_END" > $@; \
	test "$$MAX_BSS_END" -ge "$$BSS_END" || { cat $@; exit 1; }

# Check that SRAM + stack fit into the ATmega644's 4K of SRAM.
MIN_STACK_SIZE = 48
CHECKS += stack-size
check-stack-size-%.check : firmware-%.elf
	@MAX_BSS_END="$$(printf "%d\n" 0x1100)"; \
	BSS_END="$$(nm $< | awk '/__bss_end/ { print (strtonum("0x"$$1) % 0x10000); }')"; \
	STACK_START="$$(nm $< | awk '/__stack/ { print (strtonum("0x"$$1)); }')"; \
	SUM="$$(expr $$BSS_END + $(MIN_STACK_SIZE))"; \
	printf "CHECK($*): STACK_START=0x%x >= 0x%x = (BSS_END=0x%x + MIN_STACK_SIZE=0x%x)\n" \
		"$$STACK_START" "$$SUM" "$$BSS_END" "$(MIN_STACK_SIZE)" > $@; \
	test "$$STACK_START" -ge "$$SUM" || { cat $@; exit 1; }

# Caution: The "data" number includes FUSES and other things besides the .data section.
CHECKS += avr-size
check-avr-size-%.check: firmware-%.elf
	@{ echo "CHECK($*): avr-size $<"; avr-size $<; } > $@

check-ALL-%.check: $(foreach CHECK, $(CHECKS), check-$(CHECK)-%.check)
	@cat $^ > $@

check-ALL-%: check-ALL-%.check
	@cat $<

checks: $(foreach TARGET, $(TARGETS), check-ALL-$(TARGET))

# Compile: create object files from C source files.
.c.o:
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $< -o $@


# Compile: create assembler files from C source files.
.c.s:
	$(CC) -S $(CFLAGS) $(CPPFLAGS) $< -o $@


# Assemble: create object files from assembler source files.
.S.o:
	$(CC) -c $(ASFLAGS) $(CPPFLAGS) $< -o $@

CLEANFILES += invented-histogram.bin
invented-histogram.bin: ../emulator/histogram_emulator.erl
	$(MAKE) -C ../emulator histogram_emulator.beam
	$(ERL) -noshell -pa ../emulator -s histogram_emulator to_file 256 "$@" -s init stop

invented-histogram.o: invented-histogram.bin
	$(OBJCOPY) -B avr -I binary -O elf32-avr --readonly-text \
		--rename-section .data=.text,contents,alloc,load,readonly,code \
		"$<" "$@"

%.d: %.S
	@set -e; rm -f $@; \
		$(CC) -MM $(CPPFLAGS) $< > $@.$$$$; \
		sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

%.d: %.c
	@set -e; rm -f $@; \
		$(CC) -MM $(CPPFLAGS) $< > $@.$$$$; \
		sed 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

ALL_CSOURCES = $(wildcard *.c)
ALL_ASOURCES = $(wildcard *.S)
include $(ALL_CSOURCES:.c=.d) $(ALL_ASOURCES:.S=.d)

# Target: clean project.
clean:
	rm -f $(CLEANFILES)
	rm -f $(COMMON_OBJ) $(OTHER_OBJ)
	rm -f *.hex *.eep *.cof *.elf *.map *.sym *.lss
	rm -f check-*.check
	rm -f *.lst
	rm -f *.o
	rm -f *.d

.PHONY:	all elf hex eep lss sym program coff extcoff clean fuses compare-fuses program-fuses read-fuses ALL

# End of GNUmakefile
