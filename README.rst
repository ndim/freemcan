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

  * GNU make
  * avr-gcc based AVR toolchain
  * POSIX/GNU/Linux/Unix host system
  * C compiler for host system



The License
-----------

(FIXME) NL.


Hacking
-------


Ideas
-----

  * cbi/sbi do not modify SREG. That makes it easy to write an ISR
    without saving any registers, like e.g.::

       foo_vector:         /* ISR entry: 5 clock cycles */
                 sbi foo,bar          /* 2 clock cycles */
                 reti                 /* 5 clock cycles */

    for doing the reset timing stuff, potentially at ADC trigger, and
    after timer IRQ counted delay later, or similar stuff.
