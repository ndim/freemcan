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
#
# GNUmakefile structure:
#   1. Variable definitions
#   2. Default target
#   3. Explicit rules
#   4. Implicit/pattern rules and phony targets


CFLAGS =
CLEANFILES =
COMMON_OBJ =
COMMON_X =
PERSONALITIES =

include ../common.mk

# Define local settings by defining them in settings.mk. For an example of
# settings to define in settings.mk, see the settings.mk.sample
# file.
#
# If you have not defined the required things in your local
# settings.mk, the build will abort with an appropriate error message.
-include settings.mk

# Function to mark required settings.mk variables
DEFINITION_REQUIRED = \
	$(if $($(1)),\
	$(info LOCAL: $(1)=$($(1))),\
	$(error LOCAL build config variable $(1) is undefined. You may want to add/uncomment a few lines in your settings.mk file (copy settings.mk.sample if settings.mk does not exist)))

# List of all required LOCAL build config variables settings.mk needs
# to define.
$(call DEFINITION_REQUIRED,AVRDUDE_PORT)
$(call DEFINITION_REQUIRED,AVRDUDE_PROGRAMMER)
$(call DEFINITION_REQUIRED,F_CPU)
$(call DEFINITION_REQUIRED,MCU)

# End of local settings


# The firmware format we eventually feed to avrdude
FORMAT = ihex


########################################################################
# Compilation flags


# Optimize for size (or whatever gcc understands by that)
CFLAGS += -Os


# Our source code requires the F_CPU macro to be defined at all times
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
# CPPFLAGS +=


# Place -I options here
CPPFLAGS += -I../include


CPPFLAGS += -Wall -Wextra
CFLAGS += -Wstrict-prototypes
# Some more flags that might be useful for tuning:
# CFLAGS += -funsigned-char
# CFLAGS += -funsigned-bitfields

# Pack structure members together without holes
# CFLAGS += -fpack-struct

# Use 8bit integers for enum values (insted of standard int size)
CFLAGS += -fshort-enums

# Use only 8bit for the stack pointer
# CFLAGS += -mtiny-stack

# Violates the C standard, but gives sizeof(int) == sizeof(char) == 1
# (segfaults the compiler)
# CFLAGS += -mint8

# Generate error on multiple definition of variable without "extern"
CFLAGS += -fno-common

# Put extra commentary into the generated assembly code
# CFLAGS += -fverbose-asm

# Never generate code which refers to register r7
# CFLAGS += -ffixed-r7

# Called functions may clobber register r8
# CFLAGS += -fcall-used-r8

# Caller must save register r8
# CFLAGS += -fcall-saved-r9

# Make function prologues/epilogues subroutine calls
# (smaller code size? not really.)
# CFLAGS += -mcall-prologues

# Generate listing file
# CFLAGS += -Wa,-adhlns=$(<:.c=.lst)


ASFLAGS += -Wa,-adhlns=$(<:.S=.lst),-gstabs


# Add target processor to flags.
CPPFLAGS += -mmcu=$(MCU)


CPPFLAGS += -I.
ASFLAGS += -x assembler-with-cpp


########################################################################
# Link flags


# Additional libraries.

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
# Programming support using avrdude.

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
# Default target

.PHONY: all
all: elf hex lss sym fuses checks

# Legacy target
.PHONY: ALL
ALL: all


########################################################################
# Optional firmware features

ifeq ($(BUILD_FREEMCAN_PRINTF),yes)
COMMON_OBJ += .objs/uart-printf.o
CPPFLAGS += -DHAVE_UPRINTF_IMPLEMENTATION
endif

# BUG: Needs proper hooking up with new unified-firmware build system
ifeq ($(BUILD_FREEMCAN_INVENTED_HISTOGRAM),yes)
COMMON_OBJ += .objs/invented-histogram.o
CPPFLAGS += -DINVENTED_HISTOGRAM
endif

CLEANFILES += invented-histogram.bin
.objs/invented-histogram.bin: ../emulator/histogram_emulator.erl
	$(MAKE) -C ../emulator histogram_emulator.beam
	$(ERL) -noshell -pa ../emulator -s histogram_emulator to_file 256 "$@" -s init stop

.objs/invented-histogram.o: .objs/invented-histogram.bin
	$(OBJCOPY) -B avr -I binary -O elf32-avr --readonly-text \
		--rename-section .data=.text,contents,alloc,load,readonly,code \
		"$<" "$@"


########################################################################
# Link firmware images with different personalities


# Define the common object files.
# The object files specific to firmware personalities will not be listed here.
COMMON_OBJ += .objs/main.o
COMMON_OBJ += .objs/checksum.o
COMMON_OBJ += .objs/uart-comm.o
COMMON_OBJ += .objs/frame-comm.o
COMMON_OBJ += .objs/packet-comm.o
COMMON_OBJ += .objs/wdt-softreset.o
COMMON_OBJ += .objs/software-version.o
COMMON_OBJ += .objs/switch.o


# Link time checks
# LDFLAGS += -Wl,--verbose
LDFLAGS += -Wl,--defsym=RAM_END=$(RAM_END)
LDFLAGS += -Wl,--defsym=MIN_STACK_SIZE=$(MIN_STACK_SIZE)
LDFLAGS += -Wl,--defsym=MALLOC_HEAP_SIZE=$(MALLOC_HEAP_SIZE)

# Check that (data+stack) fit into SRAM with a linker script ASSERT().
# Requires RAM_END and MIN_STACK_SIZE to be --defsym'd.
COMMON_X += check-data-stack-in-SRAM.x


# ======================================================================
# Firmware personality: MCA using internal ADC
PERSONALITIES += adc-int-mca
firmware-adc-int-mca.elf : $(COMMON_OBJ) $(COMMON_X)
firmware-adc-int-mca.elf : .objs/timer1-init-simple.o
firmware-adc-int-mca.elf : .objs/timer1-countdown-and-stop.o
firmware-adc-int-mca.elf : .objs/timer1-get-duration.o
firmware-adc-int-mca.elf : .objs/perso-adc-int-mca-ext-trig.o
	$(LINK.c) $^ $(LDLIBS) --output $@

# ======================================================================
# Firmware personality: MCA using internal ADC, timed trigger
PERSONALITIES += adc-int-mca-timed
firmware-adc-int-mca-timed.elf : $(COMMON_OBJ) $(COMMON_X)
firmware-adc-int-mca-timed.elf : .objs/timer1-adc-trigger.o
firmware-adc-int-mca-timed.elf : .objs/perso-adc-int-mca-timed-trig.o
	$(LINK.c) $^ $(LDLIBS) --output $@

# ======================================================================
# Firmware personality: Sample recording using internal ADC, timed trigger
PERSONALITIES += adc-int-timed-sampling
firmware-adc-int-timed-sampling.elf : LDFLAGS += -T data-table-all-other-memory.x
firmware-adc-int-timed-sampling.elf : data-table-all-other-memory.x
firmware-adc-int-timed-sampling.elf : $(COMMON_OBJ) $(COMMON_X)
firmware-adc-int-timed-sampling.elf : .objs/timer1-adc-trigger.o
firmware-adc-int-timed-sampling.elf : .objs/data-table-all-other-memory.o
firmware-adc-int-timed-sampling.elf : .objs/perso-adc-int-log-timed-trig.o
	$(LINK.c) $(filter-out %.x,$^) $(LDLIBS) --output $@

# ======================================================================
# Firmware personality: Geiger Counter with time series
PERSONALITIES += geiger-ts
firmware-geiger-ts.elf : LDFLAGS += -T data-table-all-other-memory.x
firmware-geiger-ts.elf : data-table-all-other-memory.x
firmware-geiger-ts.elf : $(COMMON_OBJ) $(COMMON_X)
firmware-geiger-ts.elf : .objs/timer1-init-simple.o
firmware-geiger-ts.elf : .objs/timer1-get-duration.o
firmware-geiger-ts.elf : .objs/data-table-all-other-memory.o
firmware-geiger-ts.elf : .objs/perso-geiger-time-series.o
firmware-geiger-ts.elf : .objs/beep.o
	$(LINK.c) $(filter-out %.x,$^) $(LDLIBS) --output $@


########################################################################
# "metatargets"

.PHONY: elf hex eep lss sym fuses
elf: $(foreach PERSONALITY, $(PERSONALITIES), firmware-$(PERSONALITY).elf)
hex: $(foreach PERSONALITY, $(PERSONALITIES), firmware-$(PERSONALITY).hex)
eep: $(foreach PERSONALITY, $(PERSONALITIES), firmware-$(PERSONALITY).eep)
lss: $(foreach PERSONALITY, $(PERSONALITIES), firmware-$(PERSONALITY).lss)
sym: $(foreach PERSONALITY, $(PERSONALITIES), firmware-$(PERSONALITY).sym)
fuses: $(foreach PERSONALITY, $(PERSONALITIES), firmware-$(PERSONALITY).fuses)


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

.PHONY: compare-fuses program-fuses
compare-fuses program-fuses:
	$(MAKE) "$@-$(PERSONALITY)"


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
MIN_STACK_SIZE = 256

# Check that the minimum stack size we want accomodated in a firmware
# (used for other checks) is actually less than the number of push
# instructions in the firmware image. As the firmwares do not use
# recursion, the push instruction count is a good upper bound estimate
# for the stack depth.
CHECKS += min-stack-size
check-min-stack-size-%.check: firmware-%.lss
	@set -e; \
	CALL_INSTRUCTION_COUNT="$$(grep -E "\s[0-9a-f]+:\s.*\scall\s" $< | wc -l)"; \
	RCALL_INSTRUCTION_COUNT="$$(grep -E "\s[0-9a-f]+:\s.*\srcall\s" $< | wc -l)"; \
	ALL_CALL_INSTRUCTION_COUNT="$$(expr "$$CALL_INSTRUCTION_COUNT" + "$$RCALL_INSTRUCTION_COUNT")"; \
	PUSH_INSTRUCTION_COUNT="$$(grep -E "\s[0-9a-f]+:\s.*\spush\sr[0-9]+" $< | wc -l)"; \
	TOTAL_COUNT="$$(expr "$$ALL_CALL_INSTRUCTION_COUNT" + "$$PUSH_INSTRUCTION_COUNT")"; \
	echo "CHECK($*): PUSH_COUNT+2*CALL_COUNT=$$PUSH_INSTRUCTION_COUNT+2*$$ALL_CALL_INSTRUCTION_COUNT=$$TOTAL_COUNT <= MIN_STACK_SIZE=$(MIN_STACK_SIZE)"> $@; \
	test "$$TOTAL_COUNT" -le "$(MIN_STACK_SIZE)" || { cat $@; exit 1; }

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
# Firmware image formats and firmware image statistics

FIRMWARE_FORMATS += hex
firmware-%.hex: firmware-%.elf
	$(OBJCOPY) -O $(FORMAT) -R .eeprom -R .fuse $< $@

# FIRMWARE_FORMATS += eep
# firmware-%.eep: firmware-%.elf
# 	-$(OBJCOPY) -j .eeprom --set-section-flags=.eeprom="alloc,load" \
# 		--change-section-lma .eeprom=0 -O $(FORMAT) $< $@

# Create extended listing file from ELF output file.
FIRMWARE_FORMATS += lss
firmware-%.lss: firmware-%.elf
	$(OBJDUMP) -h -S $< > $@

# Create a symbol table from ELF output file.
FIRMWARE_FORMATS += sym
firmware-%.sym: firmware-%.elf
	$(NM) -n $< > $@


########################################################################
# Program the device with a firmware image

program-% : PERSONALITY = $*
program-%: $(foreach FORMAT, $(FIRMWARE_FORMATS), firmware-%.$(FORMAT)) check-ALL-%
	$(AVRDUDE) $(AVRDUDE_FLAGS) $(AVRDUDE_WRITE_FLASH) $(AVRDUDE_WRITE_EEPROM)

.PHONY: program program-eeprom
program program-eeprom:
	$(MAKE) "$@-$(PERSONALITY)"


########################################################################
# Compilation rules

# Compile: create object files from C source files.
.objs/%.o: %.c
	@$(MKDIR_P) $(@D)
	$(COMPILE.c) $< -o $@


# Assemble: create object files from assembler source files.
.objs/%.o: %.S
	@$(MKDIR_P) $(@D)
	$(CC) -c $(ASFLAGS) $(CPPFLAGS) $< -o $@


########################################################################
# Automatic dependency generation

.deps/%.S.dep: %.S
	@set -e; rm -f $@; \
		$(MKDIR_P) $(@D); \
		$(CC) -MM $(CPPFLAGS) $< > $@.$$$$; \
		$(SED) 's,\($*\)\.o[ :]*,.objs/\1.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

.deps/%.c.dep: %.c
	@set -e; rm -f $@; \
		$(MKDIR_P) $(@D); \
		$(COMPILE.c) -MM $< > $@.$$$$; \
		$(SED) 's,\($*\)\.o[ :]*,.objs/\1.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$

include $(foreach F, $(wildcard *.c *.S), .deps/$(F).dep)


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
	rm -f *~
	rm -rf .deps .objs


########################################################################
# End of GNUmakefile
