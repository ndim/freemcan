/** \file firmware/timer2-debounce-switch.c
 * \brief Debounce switch using timer2
 *
 * \author Copyright (C) 2010 samplemaker
 * \author Copyright (C) 2010 Hans Ulrich Niedermann <hun@n-dimensional.de>
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 2.1
 *  of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free
 *  Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 *  Boston, MA 02110-1301 USA
 *
 * \defgroup timer2_debounce_switch Debounce switch using timer2
 * \ingroup firmware_generic
 *
 * Debounce switch using timer2.
 *
 * @{
 */


#include <avr/io.h>
#include <avr/interrupt.h>

#include "global.h"
#include "timer2-debounce-switch.h"


/** Timer prescaler value for frequency calculations */
#define TIMER2_CLOCK_DIVISION_FACTOR 1024ULL


/** Timer prescaler value for writing to the CS02:CS00 bits
 */
#if   (TIMER2_CLOCK_DIVISION_FACTOR == 1024ULL)
# define TIMER2_CLOCK_PRESCALER_SELECT 5
#elif (TIMER2_CLOCK_DIVISION_FACTOR == 256ULL)
# define TIMER2_CLOCK_PRESCALER_SELECT 4
#elif (TIMER2_CLOCK_DIVISION_FACTOR == 64ULL)
# define TIMER2_CLOCK_PRESCALER_SELECT 3
#elif (TIMER2_CLOCK_DIVISION_FACTOR == 8ULL)
# define TIMER2_CLOCK_PRESCALER_SELECT 2
#elif (TIMER2_CLOCK_DIVISION_FACTOR == 1ULL)
# define TIMER2_CLOCK_PRESCALER_SELECT 1
#else
# error Invalid TIMER2_CLOCK_DIVISION_FACTOR value!
#endif


/** Shift register width
 *
 * Must be the same as the size of the #switch_is_inactive variable in
 * bits!
 */
#define SHIFT_REGISTER_WIDTH 16ULL


/** Debounce period in milliseconds
 *
 * This is the time it should at least take for a bit to move through
 * the shift register.
 */
#define DEBOUNCE_PERIOD_MS 100ULL


/** Approximate ISR trigger frequency in Hertz */
#define TIMER2_ISR_HZ 1024ULL


/** timer2 comparison value A
 *
 * The 1000ULL is for the millisecond unit.
 *
 * Also do a few preprocessor time range checks.
 */
#define TIMER2_COMPA_VALUE                                     \
  ((((F_CPU)*(DEBOUNCE_PERIOD_MS)-1) /                               \
    ((TIMER2_CLOCK_DIVISION_FACTOR) * 1000ULL * (SHIFT_REGISTER_WIDTH))) \
   +1)

#if (TIMER2_COMPA_VALUE < 10)
# error TIMER2_COMPA_VALUE: timer2 accuracy too low
#endif

#if (TIMER2_COMPA_VALUE > 255)
# error TIMER2_COMPA_VALUE: value exceeds range
#endif


/** The real debounce period, taking into account rounding errors etc. */
#define REAL_DEBOUNCE_PERIOD_MS                                         \
  (((SHIFT_REGISTER_WIDTH) * (TIMER2_COMPA_VALUE) * 1000ULL * (TIMER2_CLOCK_DIVISION_FACTOR)) / (F_CPU))


/* If you want to know what the actual debounce period is, uncomment
 * the following variable definition and run the following commands:
 *
 * make timer2-debounce-switch.o
 * avr-objcopy -j .ignore.me -O binary timer2-debounce-switch.o timer2-debounce-switch.period
 * od -An -t u4 timer2-debounce-switch.period
 *
 * uint32_t real_debounce_period_ms __attribute__((section(".ignore.me"))) = REAL_DEBOUNCE_PERIOD_MS;
*/


/** Run timer2 in CTC mode */
#define TIMER2_WGM_VALUE 2


/** Configure used pins */
void timer2_io_init_used_pins(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init5")));
void timer2_io_init_used_pins(void)
{
  /* PB0 is to be active-low input pin, with internal pullup */
  DDRB &= ~_BV(PB0); /* PB0 is input pin */
  PORTB |= _BV(PB0); /* activate internal pullup */
}


/** Set up and start timer2
 *
 * \bug This does not work - the main program needs to enable
 *      interupts (sei()) which it does not/cannot do (yet).
 */
void timer2_init_and_start(void)
  __attribute__ ((naked))
  __attribute__ ((section(".init7")));
void timer2_init_and_start(void)
{
  /** timer counter */
  TCNT2  = 0;
  /** output compare A value */
  OCR2A  = TIMER2_COMPA_VALUE;
  /** output compare match A interrupt enable */
  TIMSK2 = _BV(OCIE2A);
  /** we do not use timer2's asynchronous mode */
  ASSR   = 0;
  /** set up timer2 mode, and start it with appropriate prescale value */
  TCCR2A =
    BITF(TIMER2_WGM_VALUE, WGM2, 0) |
    BITF(TIMER2_WGM_VALUE, WGM2, 1);
  TCCR2B =
    BITF(TIMER2_WGM_VALUE, WGM2, 2) |
    BITF(TIMER2_CLOCK_PRESCALER_SELECT,  CS2,  2) |
    BITF(TIMER2_CLOCK_PRESCALER_SELECT,  CS2,  1) |
    BITF(TIMER2_CLOCK_PRESCALER_SELECT,  CS2,  0);
}


/** Stop timer2 instantly by turning off its clock, and set the switch
 * flag to a final "switch is dead" value.
 */
void timer2_stop(void)
{
  TCCR2B &= ~(_BV(CS22)|_BV(CS21)|_BV(CS20));
  /** Setting #switch_is_inactive to a non-zero value here is very
   * important, as the main loop will rely on #switch_is_inactive
   * signalling no pressed switch.
   */
  switch_is_inactive = 0xdead;
}


/** \internal Shift register. Usage documented in timer2-debounce-switch.h.
 *
 * This has 16 bits, so we sample the switch value during 16
 * consecutive timer2 ISR calls, and only if all of those samples have
 * been zero, we consider the switch to be pressed.
 *
 * With a TIMER2_ISR_HZ value of about 1KHz, this means we consider
 * about 16msec of switch state. Perhaps this will need to be extended
 * further.
 */
uint16_t switch_is_inactive = 0xffff;


/** Debounce input switch at PB0
 *
 * This should be called with a frequency of about TIMER2_ISR_HZ.
 *
 * The generated AVR machine code takes 51 cycles + ISR entry + ISR
 * return aka 59 cycles total. This means that at 20MHz, of every
 * second, we spend about (1s/20E6)*1024*59 = 3.0ms in this ISR.
 */
ISR(TIMER2_COMPA_vect)
{
  /** Note that we could save a few cycles and code bytes by rewriting
   * this in assembly.  avr-gcc as usualy uses 16bit ops where 8bit
   * ops would suffice.
   */
  switch_is_inactive = (switch_is_inactive << 1) | bit_is_set(PINB, PB0);
}


/* The assembly version of ISR(TIMER2_COMPA_vect) below takes 29
 * cycles + ISR entry + ISR return aka 37 cycles total. This means
 * that every second at 20MHz, we would spend about (1s/20E6)*1024*37
 * = 1.9ms in this ISR.
 *
 * That is 22 cycles less than the C ISR, we could save 1.1ms
 * every. That might only start to make sense for pulse counting rates
 * on the order of >> 100/s or >> 6000/min. The GM tube only has about
 * 1/s, though... so this is very hypothetical.
 *
 * That is not much of a difference, so keeping on using the C ISR
 * makes sense.
 *
 * (If we would globally assign three registers for the ISR's
 * exclusive use, we could get only it down to 13 cycles total aka
 * 0.67ms per second).
 *
 * #include <avr/io.h>
 * #include <avr/interrupt.h>
 *
 * .global TIMER2_COMPA_vect
 *         .type TIMER2_COMPA_vect, @function
 * TIMER2_COMPA_vect:
 *
 *         ; PUSH: 8CLK
 *         push  r24                       ; 2CLK
 *         push  r25                       ; 2CLK
 *         in    r24, __SREG__             ; 2CLK
 *         push  r24                       ; 2CLK
 *
 *         ; shift register: 13CLK
 *         lds   r24, switch_is_inactive   ; 2CLK
 *         lds   r25, switch_is_inactive+1 ; 2CLK
 *
 *         clc   ; clear carry flag        ; 1CLK
 *         ; 'sbic' = Skip 'sec' if Bit in IO register is Cleared (i.e. PB0)
 *         sbic  _SFR_IO_ADDR(PINB), 0     ; 2 CLK from here until after 'sec'
 *         sec   ; set carry flag
 *         rol   r24                       ; 1CLK rotate through carry
 *         rol   r25                       ; 1CLK rotate through carry
 *
 *         sts   switch_is_inactive, r24   ; 2CLK
 *         sts   switch_is_inactive+1, r25 ; 2CLK
 *
 *         ; POP: 8CLK
 *         pop   r24                       ; 2CLK
 *         out   __SREG__, r24             ; 2CLK
 *         pop   r25                       ; 2CLK
 *         pop   r24                       ; 2CLK
 *
 *         reti
 *         .size TIMER2_COMPA_vect, . - TIMER2_COMPA_vect
 *
 */


/** @} */

/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
