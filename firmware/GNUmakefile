# Hey Emacs, this is a -*- makefile -*-

# AVR-GCC Makefile template, derived from the WinAVR template (which
# is public domain), believed to be neutral to any flavor of "make"
# (GNU make, BSD make, SysV make).
#
# Relying on GNU make features now as I can't stand the compatible
# make deficiencies.


CFLAGS =
CLEANFILES =


# Override settings by defining them in settings.mk Things not defined
# in settings.mk will be later defined with ?= later in this Makefile.
# AVRDUDE_PROGRAMMER, AVRDUDE_PORT, MCU are good candidates for
# settings.mk.
-include settings.mk


MCU ?= atmega644p
FORMAT = ihex
TARGET = firmware
SRC = main.c uart-comm.c frame-comm.c
ASRC = # ISR-ADC_vect.S
OPT = s
BSRC = invented-histogram.bin

# Comment out the following two files to remove the printf implementation
# from the firmware.
# SRC += uart-printf.c
# CFLAGS += -DHAVE_UPRINTF_IMPLEMENTATION

# Name of this Makefile (used for "make depend").
MAKEFILE = GNUmakefile

# Debugging format.
# Native formats for AVR-GCC's -g are stabs [default], or dwarf-2.
# AVR (extended) COFF requires stabs, plus an avr-objcopy run.
DEBUG = stabs

# Compiler flag to set the C Standard level.
# c89   - "ANSI" C
# gnu89 - c89 plus GCC extensions
# c99   - ISO C99 standard (not yet fully implemented)
# gnu99 - c99 plus GCC extensions
CSTANDARD = -std=gnu99

# Place -D or -U options here
CDEFS =

# Place -I options here
CINCS = -I../include


CDEBUG = -g$(DEBUG)
CWARN = -Wall -Wextra -Wstrict-prototypes
CTUNING = -funsigned-char -funsigned-bitfields -fpack-struct -fshort-enums
#CEXTRA = -Wa,-adhlns=$(<:.c=.lst)
CFLAGS += $(CDEBUG) $(CDEFS) $(CINCS) -O$(OPT) $(CWARN) $(CSTANDARD) $(CEXTRA)


ASFLAGS =
ASFLAGS += -Wa,-adhlns=$(<:.S=.lst),-gstabs
ASFLAGS += -Wall -Wextra


#Additional libraries.

# Minimalistic printf version
PRINTF_LIB_MIN = -Wl,-u,vfprintf -lprintf_min

# Floating point printf version (requires MATH_LIB = -lm below)
PRINTF_LIB_FLOAT = -Wl,-u,vfprintf -lprintf_flt

PRINTF_LIB =

# Minimalistic scanf version
SCANF_LIB_MIN = -Wl,-u,vfscanf -lscanf_min

# Floating point + %[ scanf version (requires MATH_LIB = -lm below)
SCANF_LIB_FLOAT = -Wl,-u,vfscanf -lscanf_flt

SCANF_LIB =

MATH_LIB = -lm

# External memory options

# 64 KB of external RAM, starting after internal RAM (ATmega128!),
# used for variables (.data/.bss) and heap (malloc()).
#EXTMEMOPTS = -Wl,--section-start,.data=0x801100,--defsym=__heap_end=0x80ffff

# 64 KB of external RAM, starting after internal RAM (ATmega128!),
# only used for heap (malloc()).
#EXTMEMOPTS = -Wl,--defsym=__heap_start=0x801100,--defsym=__heap_end=0x80ffff

EXTMEMOPTS =

LDMAP = -Wl,-Map=$(TARGET).map,--cref
LDFLAGS = $(EXTMEMOPTS) $(LDMAP) $(PRINTF_LIB) $(SCANF_LIB) $(MATH_LIB)


# Programming support using avrdude. Settings and variables.

AVRDUDE_PROGRAMMER ?= ponyser
AVRDUDE_PORT ?= /dev/ttyS0

AVRDUDE_WRITE_FLASH = -U flash:w:$(TARGET).hex
#AVRDUDE_WRITE_EEPROM = -U eeprom:w:$(TARGET).eep


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


CC = avr-gcc
OBJCOPY = avr-objcopy
OBJDUMP = avr-objdump
SIZE = avr-size
NM = avr-nm
AVRDUDE = avrdude
REMOVE = rm -f
MV = mv -f
OD = od

# Define all object files.
OBJ = $(SRC:.c=.o) $(ASRC:.S=.o) $(BSRC:.bin=.o)

# Define all listing files.
LST = $(ASRC:.S=.lst) $(SRC:.c=.lst)

# Combine all necessary flags and optional flags.
# Add target processor to flags.
ALL_CFLAGS = -mmcu=$(MCU) -I. $(CFLAGS)
ALL_ASFLAGS = -mmcu=$(MCU) -I. -x assembler-with-cpp $(ASFLAGS)


# Default target.
all: build

build: elf hex eep checks

ALL: elf hex eep lss sym fuses checks

elf: $(TARGET).elf
hex: $(TARGET).hex
eep: $(TARGET).eep
lss: $(TARGET).lss
sym: $(TARGET).sym
fuses: $(TARGET).fuses


# Program the device.
program: $(TARGET).hex $(TARGET).eep
	$(AVRDUDE) $(AVRDUDE_FLAGS) $(AVRDUDE_WRITE_FLASH) $(AVRDUDE_WRITE_EEPROM)


CLEANFILES += $(TARGET).fuses
CLEANFILES += $(TARGET).lfuse.txt $(TARGET).hfuse.txt $(TARGET).efuse.txt
$(TARGET).fuses: $(TARGET).elf
	$(OBJCOPY) -j .fuse -O binary $(TARGET).elf $(TARGET).fuses
	@chmod -x $(TARGET).fuses
	$(OD) -A n -t xC $(TARGET).fuses | \
	{ read lfuse hfuse efuse restofline; \
	  echo "0x$$lfuse" > $(TARGET).lfuse.txt; \
	  echo "0x$$hfuse" > $(TARGET).hfuse.txt; \
	  echo "0x$$efuse" > $(TARGET).efuse.txt; \
	}

compare-fuses: read-fuses $(TARGET).fuses
	@echo "New fuse values to be written to device:"; \
	for f in lfuse hfuse efuse; do \
		echo "    $$f=$$(cat "$(TARGET).$$f.txt")"; \
	done

program-fuses: $(TARGET).fuses
	@echo "Writing fuses to device:"; \
	for f in lfuse hfuse efuse; do \
		echo "    $$f=$$(cat "$(TARGET).$$f.txt")"; \
	done
	$(AVRDUDE) $(AVRDUDE_FLAGS) -u \
		-U lfuse:w:$(shell cat $(TARGET).lfuse.txt):m \
		-U hfuse:w:$(shell cat $(TARGET).hfuse.txt):m \
		-U efuse:w:$(shell cat $(TARGET).efuse.txt):m

read-fuses:
	$(AVRDUDE) $(AVRDUDE_FLAGS) \
		-U lfuse:r:DEVICE.lfuse.txt:h \
		-U hfuse:r:DEVICE.hfuse.txt:h \
		-U efuse:r:DEVICE.efuse.txt:h
	@echo "Read fuses from device:"; \
	for f in lfuse hfuse efuse; do \
		echo "    $$f=$$(cat "DEVICE.$$f.txt")"; \
	done

# Convert ELF to COFF for use in debugging / simulating in AVR Studio or VMLAB.
COFFCONVERT=$(OBJCOPY) --debugging \
--change-section-address .data-0x800000 \
--change-section-address .bss-0x800000 \
--change-section-address .noinit-0x800000 \
--change-section-address .eeprom-0x810000


coff: $(TARGET).elf
	$(COFFCONVERT) -O coff-avr $(TARGET).elf $(TARGET).cof


extcoff: $(TARGET).elf
	$(COFFCONVERT) -O coff-ext-avr $(TARGET).elf $(TARGET).cof


.SUFFIXES: .elf .hex .eep .lss .sym

.elf.hex:
	$(OBJCOPY) -O $(FORMAT) -R .eeprom -R .fuse $< $@

.elf.eep:
	-$(OBJCOPY) -j .eeprom --set-section-flags=.eeprom="alloc,load" \
	--change-section-lma .eeprom=0 -O $(FORMAT) $< $@

# Create extended listing file from ELF output file.
.elf.lss:
	$(OBJDUMP) -h -S $< > $@

# Create a symbol table from ELF output file.
.elf.sym:
	$(NM) -n $< > $@



# Link: create ELF output file from object files.
$(TARGET).elf: $(OBJ)
	$(CC) $(ALL_CFLAGS) $(OBJ) --output $@ $(LDFLAGS)

# Check that the SRAM usage fits into the ATmega644's 4K SRAM
# size: __bss_end must be <= 0x00801100.
#
# The 0x00800000 is avr-gcc's Harvard architecture trick, and the
# first 0x100 bytes in the addr space are CPU and IO registers, after
# which the 4K of SRAM start, up to and including address 0x10ff.
checks : check-bss-end
check-bss-end: $(TARGET).elf
	@MAX_BSS_END="$$(printf "%d\n" 0x1100)"; \
	BSS_END="$$(nm $< | awk '/__bss_end/ { print (strtonum("0x"$$1) % 0x10000); }')"; \
	printf "CHECK: MAX_BSS_END=0x%x >= BSS_END=0x%x\n" "$$MAX_BSS_END" "$$BSS_END"; \
	test "$$MAX_BSS_END" -ge "$$BSS_END"

# Check that SRAM + stack fit into the ATmega644's 4K of SRAM.
MIN_STACK_SIZE = 48
checks : check-stack-size
check-stack-size : $(TARGET).elf
	@MAX_BSS_END="$$(printf "%d\n" 0x1100)"; \
	BSS_END="$$(nm $< | awk '/__bss_end/ { print (strtonum("0x"$$1) % 0x10000); }')"; \
	STACK_START="$$(nm $< | awk '/__stack/ { print (strtonum("0x"$$1)); }')"; \
	SUM="$$(expr $$BSS_END + $(MIN_STACK_SIZE))"; \
	printf "CHECK: STACK_START=0x%x >= 0x%x = (BSS_END=0x%x + MIN_STACK_SIZE=0x%x)\n" \
		"$$STACK_START" "$$SUM" "$$BSS_END" "$(MIN_STACK_SIZE)"; \
	test "$$STACK_START" -ge "$$SUM"

# Caution: The "data" number includes FUSES and other things besides the .data section.
checks : check-avr-size
check-avr-size: $(TARGET).elf
	avr-size $<

# Compile: create object files from C source files.
.c.o:
	$(CC) -c $(ALL_CFLAGS) $< -o $@


# Compile: create assembler files from C source files.
.c.s:
	$(CC) -S $(ALL_CFLAGS) $< -o $@


# Assemble: create object files from assembler source files.
.S.o:
	$(CC) -c $(ALL_ASFLAGS) $< -o $@

invented-histogram.bin: ../emulator/histogram_emulator.erl
	$(MAKE) -C ../emulator histogram_emulator.beam
	erl -noshell -pa ../emulator -s histogram_emulator to_file 256 "$@" -s init stop

invented-histogram.o: invented-histogram.bin
	$(OBJCOPY) -B avr -I binary -O elf32-avr --readonly-text \
		--rename-section .data=.text,contents,alloc,load,readonly,code \
		"$<" "$@"

# Target: clean project.
clean:
	$(REMOVE) $(TARGET).hex $(TARGET).eep $(TARGET).cof $(TARGET).elf \
	$(TARGET).map $(TARGET).sym $(TARGET).lss \
	$(CLEANFILES) \
	$(OBJ) $(LST) $(SRC:.c=.s) $(SRC:.c=.d)

depend:
	if grep '^# DO NOT DELETE' $(MAKEFILE) >/dev/null; \
	then \
		sed -e '/^# DO NOT DELETE/,$$d' $(MAKEFILE) > \
			$(MAKEFILE).$$$$ && \
		$(MV) $(MAKEFILE).$$$$ $(MAKEFILE); \
	fi
	echo '# DO NOT DELETE THIS LINE -- make depend depends on it.' \
		>> $(MAKEFILE); \
	$(CC) -M -mmcu=$(MCU) $(CDEFS) $(CINCS) $(SRC) $(ASRC) >> $(MAKEFILE)

.PHONY:	all build elf hex eep lss sym program coff extcoff clean depend fuses compare-fuses program-fuses read-fuses ALL

main.o: main.c global.h uart-comm.h frame-comm.h \
	../include/frame-defs.h ../include/packet-defs.h
uart-comm.o: uart-comm.c uart-comm.h global.h ../include/uart-defs.h
frame-comm.o: frame-comm.c uart-comm.h frame-comm.h ../include/frame-defs.h
uart-printf.o: uart-printf.c uart-printf.h uart-comm.h frame-comm.h \
	../include/frame-defs.h


