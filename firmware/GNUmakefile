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
COMMON_X =
PERSONALITIES =

include ../common.mk

# Override settings by defining them in settings.mk. For an example of
# settings to define in settings.mk, see the settings.mk.sample
# file. Things not defined in settings.mk will be later defined with
# ?= later in this Makefile.  AVRDUDE_PROGRAMMER, AVRDUDE_PORT, F_CPU,
# MCU are good candidates for settings.mk.
-include settings.mk

MCU ?= atmega644p
F_CPU ?= 16000000UL

# End of default settings


FORMAT = ihex

PERSONALITIES += adc-ext-mca
PERSONALITIES += adc-int-mca
PERSONALITIES += geiger-counter
PERSONALITIES += geiger-ts


CFLAGS += -Os


CPPFLAGS += -DF_CPU=$(F_CPU)


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
LDFLAGS += -Wl,-Map=$(@:.elf=.map),--cref


# Define the common object files.
# The object files specific to firmware personalities will not be listed here.
COMMON_OBJ += main.o
COMMON_OBJ += uart-comm.o
COMMON_OBJ += frame-comm.o
COMMON_OBJ += packet-comm.o
COMMON_OBJ += wdt-softreset.o
COMMON_OBJ += software-version.o
COMMON_OBJ += send-table.o

# Add target processor to flags.
CPPFLAGS += -mmcu=$(MCU)

CPPFLAGS += -I.
ASFLAGS += -x assembler-with-cpp


########################################################################
# The programs used

AWK = gawk
SED = sed
ERL = erl
CC = avr-gcc
OBJCOPY = avr-objcopy
OBJDUMP = avr-objdump
SIZE = avr-size
NM = avr-nm
AVRDUDE = avrdude
OD = od


########################################################################
# Programming support using avrdude. Settings and variables.

AVRDUDE_PROGRAMMER ?= ponyser
AVRDUDE_PORT ?= /dev/ttyS0

AVRDUDE_WRITE_FLASH = -U flash:w:firmware-$(PERSONALITY).hex
#AVRDUDE_WRITE_EEPROM = -U eeprom:w:firmware-$(PERSONALITY).eep


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


########################################################################
# Default target and related "metatargets"

.PHONY: all
all: elf hex lss sym fuses checks

# Legacy target
.PHONY: ALL
ALL: all

.PHONY: elf hex eep lss sym fuses
elf: $(foreach PERSONALITY, $(PERSONALITIES), firmware-$(PERSONALITY).elf)
hex: $(foreach PERSONALITY, $(PERSONALITIES), firmware-$(PERSONALITY).hex)
eep: $(foreach PERSONALITY, $(PERSONALITIES), firmware-$(PERSONALITY).eep)
lss: $(foreach PERSONALITY, $(PERSONALITIES), firmware-$(PERSONALITY).lss)
sym: $(foreach PERSONALITY, $(PERSONALITIES), firmware-$(PERSONALITY).sym)
fuses: $(foreach PERSONALITY, $(PERSONALITIES), firmware-$(PERSONALITY).fuses)


########################################################################
# Program the device with a firmware image

program-% : PERSONALITY = $*
program-%: firmware-%.hex firmware-%.eep
	$(AVRDUDE) $(AVRDUDE_FLAGS) $(AVRDUDE_WRITE_FLASH) $(AVRDUDE_WRITE_EEPROM)


########################################################################
# AVR fuse extraction, reading, writing, comparing

CLEANFILES += $(foreach PERSONALITY, $(PERSONALITIES), firmware-$(PERSONALITY).fuses)
CLEANFILES += $(foreach PERSONALITY, $(PERSONALITIES), firmware-$(PERSONALITY).lfuse.txt)
CLEANFILES += $(foreach PERSONALITY, $(PERSONALITIES), firmware-$(PERSONALITY).hfuse.txt)
CLEANFILES += $(foreach PERSONALITY, $(PERSONALITIES), firmware-$(PERSONALITY).efuse.txt)
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
		echo "    $$f=$$(cat "$*.$$f.txt")"; \
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


########################################################################
# Optional firmware features

ifeq ($(BUILD_FREEMCAN_PRINTF),yes)
COMMON_OBJ += uart-printf.o
CPPFLAGS += -DHAVE_UPRINTF_IMPLEMENTATION
endif

# BUG: Needs proper hooking up with new unified-firmware build system
ifeq ($(BUILD_FREEMCAN_INVENTED_HISTOGRAM),yes)
COMMON_OBJ += invented-histogram.o
CPPFLAGS += -DINVENTED_HISTOGRAM
endif

CLEANFILES += invented-histogram.bin
invented-histogram.bin: ../emulator/histogram_emulator.erl
	$(MAKE) -C ../emulator histogram_emulator.beam
	$(ERL) -noshell -pa ../emulator -s histogram_emulator to_file 256 "$@" -s init stop

invented-histogram.o: invented-histogram.bin
	$(OBJCOPY) -B avr -I binary -O elf32-avr --readonly-text \
		--rename-section .data=.text,contents,alloc,load,readonly,code \
		"$<" "$@"


########################################################################
# Firmware image formats and firmware image statistics

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


########################################################################
# Link firmware images with different personalities


# Link time checks
# LDFLAGS += -Wl,--verbose
LDFLAGS += -Wl,--defsym=RAM_END=$(RAM_END)
LDFLAGS += -Wl,--defsym=MIN_STACK_SIZE=$(MIN_STACK_SIZE)
LDFLAGS += -Wl,--defsym=MALLOC_HEAP_SIZE=$(MALLOC_HEAP_SIZE)

# Check that (data+stack) fit into SRAM with a linker script ASSERT().
# Requires RAM_END and MIN_STACK_SIZE to be --defsym'd.
COMMON_X += check-data-stack-in-SRAM.x


# Firmware personality: MCA using external ADC
firmware-adc-ext-mca.elf : $(COMMON_OBJ) $(COMMON_X) measurement-timer-ISR-countdown-and-stop.o adc-ext-mca.o
	$(LINK.c) $^ $(LDLIBS) --output $@

# Firmware personality: MCA using internal ADC
firmware-adc-int-mca.elf : $(COMMON_OBJ) $(COMMON_X)  measurement-timer-ISR-countdown-and-stop.o adc-int-mca.o
	$(LINK.c) $^ $(LDLIBS) --output $@

# Firmware personality: Geiger Counter without time series
firmware-geiger-counter.elf : $(COMMON_OBJ) $(COMMON_X)  measurement-timer-ISR-countdown-and-stop.o geiger-counter.o
	$(LINK.c) $^ $(LDLIBS) --output $@

# Extract size of a single table element in bytes
CLEANFILES += geiger-time-series.element-size.txt
geiger-time-series.element-size.txt: geiger-time-series.o
	@set -e; \
	rm -f $@; \
	$(OBJCOPY) -j .data.sizeof -O binary $< $@.$$$$; \
	echo $$(od -An -t u2 $@.$$$$) > $@; \
	rm -f $@.$$$$

# Firmware personality: Geiger Counter with time series
firmware-geiger-ts.elf : LDFLAGS += -T time-series-table.x
firmware-geiger-ts.elf : time-series-table.x
firmware-geiger-ts.elf : geiger-time-series.element-size.txt
firmware-geiger-ts.elf : $(COMMON_OBJ) $(COMMON_X) geiger-time-series.o
	$(LINK.c) $(filter-out time-series-table.x %.txt,$^) $(LDLIBS) --output $@
	$(NM) $@ | $(AWK) '/data_table_size/ { printf "%5d %s\n", strtonum("0x"$$1), $$3; }'
	@DATA_TABLE_SIZE_BYTES="$$($(NM) $@ | $(AWK) '($$3 == "data_table_size") { print strtonum("0x"$$1); }')"; \
	ELEMENT_SIZE_BYTES="$$(cat geiger-time-series.element-size.txt)"; \
	DATA_TABLE_SIZE="$$(expr "$$DATA_TABLE_SIZE_BYTES" / "$$ELEMENT_SIZE_BYTES")"; \
	ELEMENT_SIZE_BIT="$$(expr 8 \* "$$ELEMENT_SIZE_BYTES")"; \
	STR="$$(printf "data table of %4d elements of %2dbit each" "$$DATA_TABLE_SIZE" "$$ELEMENT_SIZE_BIT")"; \
	echo $(SED) -b -i "s/data table of #### elements of ##bit each/$${STR}/" $@; \
	$(SED) -b -i "s/data table of #### elements of ##bit each/$${STR}/" $@

########################################################################
# Check the firmware images


# The 0x00800000 is avr-gcc's Harvard architecture trick, and the
# first 0x100 bytes in the addr space are CPU and IO registers, after
# which the 4K of SRAM start, up to and including address 0x10ff.
RAM_END = 0x00801100

# Size of memory area we reserve for malloc(3)
MALLOC_HEAP_SIZE = 64

# The minimum required stack size we want to have space for (i.e. the
# maximum stack size that can appear at runtime). We just about assume
# this value, but have a consistency check lower down in the form of
# the check-min-stack-size-%.check target.
MIN_STACK_SIZE = 80

# Check that the minimum stack size we want accomodated in a firmware
# (used for other checks) is actually less than the number of push
# instructions in the firmware image. As the firmwares do not use
# recursion, the push instruction count is a good upper bound estimate
# for the stack depth.
CHECKS += min-stack-size
check-min-stack-size-%.check: firmware-%.lss
	@set -e; \
	PUSH_INSTRUCTION_COUNT="$$(grep -E "\s[0-9a-f]+:\s.*\spush\sr[0-9]+" $< | wc -l)"; \
	echo "CHECK($*): PUSH_INSTRUCTION_COUNT=$$PUSH_INSTRUCTION_COUNT <= MIN_STACK_SIZE=$(MIN_STACK_SIZE)"> $@; \
	test "$$PUSH_INSTRUCTION_COUNT" -le "$(MIN_STACK_SIZE)" || { cat $@; exit 1; }

check-ALL-%.check: $(foreach CHECK, $(CHECKS), check-$(CHECK)-%.check)
	@cat $^ > $@

check-ALL-%: check-ALL-%.check
	@cat $<

# The awk magic sums up the numbers from the text and data columns,
# i.e. the total amount of bytes that need to be flashed to the chip,
# and shows them under the new "txt+dat" column.
.PHONY: checks
checks: $(foreach PERSONALITY, $(PERSONALITIES), check-ALL-$(PERSONALITY))
	@$(SIZE) $(foreach PERSONALITY, $(PERSONALITIES), firmware-$(PERSONALITY).elf) \
	| $(AWK) '($$1 ~ /^[0-9]+$$/) { s=$$1+$$2; printf "%7d\t%s\n", s, $$0; next; } { printf "%7s\t%s\n", "txt+dat", $$0; }'


########################################################################
# Compilation rules

# Compile: create object files from C source files.
.c.o:
	$(COMPILE.c) $< -o $@


# Assemble: create object files from assembler source files.
.S.o:
	$(CC) -c $(ASFLAGS) $(CPPFLAGS) $< -o $@


########################################################################
# Automatic dependency generation

%.d: %.S
	@set -e; rm -f $@; \
		$(CC) -MM $(CPPFLAGS) $< > $@.$$$$; \
		$(SED) 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

%.d: %.c
	@set -e; rm -f $@; \
		$(COMPILE.c) -MM $< > $@.$$$$; \
		$(SED) 's,\($*\)\.o[ :]*,\1.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

ALL_CSOURCES = $(wildcard *.c)
ALL_ASOURCES = $(wildcard *.S)
include $(ALL_CSOURCES:.c=.d) $(ALL_ASOURCES:.S=.d)


########################################################################
# clean target

.PHONY: clean
clean:
	rm -f $(CLEANFILES)
	rm -f $(COMMON_OBJ) $(OTHER_OBJ)
	rm -f *.hex *.eep *.cof *.elf *.map *.sym *.lss
	rm -f check-*.check
	rm -f *.lst
	rm -f *.o
	rm -f *.d *.d.[0-9]*


########################################################################
# End of GNUmakefile
