# freemcan


## What is freemcan?

freemcan is software for using custom gamma spectrum analyzer and
geiger counter hardware. It consists of both the firmware to run on
the custom hardware and of hostware to run a Linux PC.

As the custom hardware contains a high voltage generator, we have
not published the schematics or board files for its hardware yet.

[![Build Status](https://travis-ci.org/ndim/freemcan.svg?branch=master)](https://travis-ci.org/ndim/freemcan)


### Why the name?

  * We wanted it to be Free Software.

  * MCA is the acronym for Multi Channel Analyzer which is the common
    term for the device.

  * "freemca" produced a few hits on Google. "freemcan" did not.
    Brand name collision wise, an unused word appeared to be the
    better choice.


### The Plan

(FIXME) No plan.


## Building

For a reasonably quick build, we recommend a parallel build:

    $ make -j -O -l 5

Otherwise, you can build serially with

    $ make

Installation is not supported at this time.


### Software Requirements


  * [GNU make](http://www.gnu.org/software/make/)

  * [avr-gcc](http://gcc.gnu.org/) based AVR toolchain

  * [avr-binutils](http://sourceware.org/binutils/) >= 2.19
    (we use INSERT AFTER in linker scripts)

  * POSIX/GNU/Linux/Unix host system

  * [gcc](http://gcc.gnu.org/) compiler for host system

For building the internal code documentation (mostly of interest to
freemcan developers), you additionally need

  * [doxygen](http://www.stack.nl/~dimitri/doxygen/index.html)

  * [graphviz](http://www.graphviz.org/)

  * [sloccount](http://www.dwheeler.com/sloccount)

    Optionally, for creating the source lines of code (SLOC) summary.


## Usage

TBA.


## The License

LGPLv2.1+


## Hacking


### Subdirectory Contents

  * `firmware/`

    The device firmware for Atmel ATmega644 microcontroller

  * `code-comparison/`

    Some common tasks our firmware needs written in portable C
    and compiled for all platforms we have a cross compiler
    for. This lets us compare the assembly language generated
    for those platforms.

  * `hostware/`

    All the software running on the PC host. For lack of a
    better word, we called it "hostware" to distinguish it from
    the "firmware".

  * `emulator/`

    Simple attempt at emulating the device connected to a
    device file by having an Erlang program connected to a
    Unix domain socket.


## Ideas

  * cbi/sbi do not modify SREG. That makes it easy to write an ISR
    without saving any registers, like e.g.:

        foo_vector:           /* enter ISR: 5 clock cycles */
                sbi foo,bar              /* 2 clock cycles */
                reti          /* leave ISR: 5 clock cycles */

    for doing the reset timing stuff, potentially at ADC trigger, and
    after timer IRQ counted delay later, or similar stuff.


## Known issues

  * `ccache` is known to not generate the `*.i` and `*.s` when called
    with the `-save-temps=obj` option.  Until your local `ccache` has
    been fixed, set `CCACHE_DISABLE=true` or remove `/usr/lib/ccache`
    or `/usr/lib64/ccache` from the `PATH` as a workaround.

    The same issue applies to other files like linker map files
    (`-Wl,-Map=$(@:.elf=.map),--cref`) and assembly listing files
    (`-Wa,-adhlns=$(@:.o=.lst)`).
