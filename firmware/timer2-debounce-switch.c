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
 * \ingroup firmware
 *
 * Debounce switch using timer2.
 *
 * @{
 */


#include <avr/io.h>
#include <avr/interrupt.h>

#include "timer2-debounce-switch.h"


/** Timer prescaler value for frequency calculations */
#define TIMER2_PRESCALE_FACTOR 1024ULL


/** Timer prescaler value for writing to the CS02:CS00 bits
 */
#if   (TIMER2_PRESCALE_FACTOR == 1024ULL)
# define TIMER2_CS_VALUE 5
#elif (TIMER2_PRESCALE_FACTOR == 256ULL)
# define TIMER2_CS_VALUE 4
#elif (TIMER2_PRESCALE_FACTOR == 64ULL)
# define TIMER2_CS_VALUE 3
#elif (TIMER2_PRESCALE_FACTOR == 8ULL)
# define TIMER2_CS_VALUE 2
#elif (TIMER2_PRESCALE_FACTOR == 1ULL)
# define TIMER2_CS_VALUE 1
#else
# error Invalid TIMER2_PRESCALE_FACTOR value!
#endif


/** Shift register width
 *
 * Must be the same as sizeof(switch_is_inactive)!
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
  (((F_CPU*DEBOUNCE_PERIOD_MS-1) /                             \
    (TIMER2_PRESCALE_FACTOR * 1000ULL * SHIFT_REGISTER_WIDTH)) \
   +1)

#if (TIMER2_COMPA_VALUE < 10)
# error TIMER2_COMPA_VALUE: timer2 accuracy too low
#endif

#if (TIMER2_COMPA_VALUE > 255)
# error TIMER2_COMPA_VALUE: value exceeds range
#endif


/** The real debounce period, taking into account rounding errors etc. */
#define REAL_DEBOUNCE_PERIOD_MS                                         \
  ((SHIFT_REGISTER_WIDTH * TIMER2_COMPA_VALUE * 1000ULL * TIMER2_PRESCALE_FACTOR) / F_CPU)


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


/** Convenience macro for composing bit fields in registers
 *
 * \param VALUE   A value like e.g. 5. Can be a macro.
 * \param BITNAME Bit base name, e.g. WGM0 for the WGM0x bits.
 * \param BITNO   Bit number, e.g. 2 for the WGM02 bit.
 *                Must not be a macro.
 */
#define BITF(VALUE, BITNAME, BITNO)			\
  ((((VALUE)>>BITNO)&1) * (1<<(BITNAME##BITNO)))


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
    BITF(TIMER2_CS_VALUE,  CS2,  2) |
    BITF(TIMER2_CS_VALUE,  CS2,  1) |
    BITF(TIMER2_CS_VALUE,  CS2,  0);
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
 */
ISR(TIMER2_COMPA_vect)
{
  /** Note that we could save a few cycles and code bytes by rewriting
   * this in assembly.  avr-gcc as usualy uses 16bit ops where 8bit
   * ops would suffice.
   */
  switch_is_inactive = (switch_is_inactive << 1) | bit_is_set(PORTB, PB0);
}


/** @} */

/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
