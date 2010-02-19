freemcan
========

.. contents::



What is freemcan?
-----------------

(FIXME) freemcan is stuff.


Why the name?
~~~~~~~~~~~~~

  * We wanted it to be Free Software.
  * MCA is the acronym for Multi Channel Analyzer which is the common
    term for the device.
  * "freemca" produced a few hits on Google. "freemcan" did not, so
    brand name collision wise an unused word appeared to be wise to
    choose.



The Plan
~~~~~~~~

(FIXME) No plan.




Building
--------

  $ make

Installation is not supported at this time.


Software Requirements
~~~~~~~~~~~~~~~~~~~~~

  * GNU make_
  * avr-gcc_ based AVR toolchain
  * POSIX/GNU/Linux/Unix host system
  * gcc_ compiler for host system

For building the internal code documentation (mostly of interest to
hackers), you additionally need

  * doxygen_
  * graphviz_

.. _avr-gcc:   http://gcc.gnu.org/
.. _doxygen:   http://www.stack.nl/~dimitri/doxygen/index.html
.. _gcc:       http://gcc.gnu.org/
.. _graphviz:  http://www.graphviz.org/
.. _make:      http://www.gnu.org/software/make/



Usage
-----

After building, you need to write the firmware to your hardware. To do that,
generate a file firmware/atmega/settings.mk with your local avrdude settings,
such as e.g.

    AVRDUDE_PROGRAMMER = ponyser
    AVRDUDE_PORT = /dev/ttyS0

You can also use all GNU make syntax in that file to set those
variables, e.g. if you need to determine AVRDUDE_PORT via running a
shell script.

Then

  $ cd firmware/atmega

and run the local make targets to write the firmware to your hardware.



The License
-----------

(FIXME) Add copyright and license headers to source files.
(FIXME) Add license text as separate file.



Hacking
-------


Subdirectory Contents
~~~~~~~~~~~~~~~~~~~~~


   firmware/
           Everything firmware related.

   firmware/atmega/
           The device firmware for Atmel ATmega AVR8 microcontroller.

   firmware/code-comparison/
           Some common tasks our firmware needs written in portable C
           and compiled for all platforms we have a cross compiler
           for. This lets us compare the assembly language generated
           for those platforms.

   hostware/
           All the software running on the PC host. For lack of a
           better word, we called it "hostware" to distinguish it from
           the "firmware":



Ideas
-----

  * cbi/sbi do not modify SREG. That makes it easy to write an ISR
    without saving any registers, like e.g.::

       foo_vector:         /* ISR entry: 5 clock cycles */
                 sbi foo,bar          /* 2 clock cycles */
                 reti                 /* 5 clock cycles */

    for doing the reset timing stuff, potentially at ADC trigger, and
    after timer IRQ counted delay later, or similar stuff.
