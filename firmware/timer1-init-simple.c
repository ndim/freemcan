/** \file firmware/timer1-init-simple.c
 * \brief Timer init to simply periodically trigger timer ISR
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
 * \defgroup timer1_init_simple Timer init to simply periodically trigger timer ISR
 * \ingroup firmware_personality_groups
 *
 * Timer init to simply periodically trigger timer ISR.
 *
 * @{
 */


#include "data-table.h"
#include "init-functions.h"
#include "timer1-constants.h"
#include "timer1-measurement.h"


/** Timer1 compare output mode for channel A for non-PWM mode:
 *
 * Toggle LED pin 19 on ATmega644 DIP40 on compare match.
 */
#define TIMER1_COMA_MODE 1


/** Set up our IO pins */
INIT_FUNCTION(init5, timer1_simple_io_init)
{
  /* Configure "measurement in progress LED"                      */
  /* configure ATmega644 pin 19 as an output */
  DDRD |= (_BV(DDD5));
  /* Light up LED on PD5. Will be periodically toggled with different
   * blinking patterns during and after measurement. */
  PORTD |= _BV(PD5);
}


/** Define static string in a single place */
const char PSTR_UNSUPPORTED_TIMER_VALUE_LE_1[] PROGMEM =
  "Unsupported timer value <= 1";


/** Configure 16 bit timer to trigger an ISR every second
 *
 * Configure "measurement in progress toggle LED-signal"
 */
void timer1_init(const uint16_t timer1_value)
{
  orig_timer1_count = timer1_value;
  timer1_count = timer1_value;

  /** Safeguard: We cannot handle 0 or 1 count measurements. */
  if (orig_timer1_count <= 1) {
    send_text_P(PSTR_UNSUPPORTED_TIMER_VALUE_LE_1);
    wdt_soft_reset();
  }

  /* Compare match value into output compare reg. A               */
  OCR1A = TIMER1_COMPARE_MATCH_VAL;

  /* Configure and start timer */
  TCCR1A =
    BITF(TIMER1_COMA_MODE, COM1A, 0) |
    BITF(TIMER1_COMA_MODE, COM1A, 1) |
    BITF(TIMER1_WGM_MODE,  WGM1, 0) |
    BITF(TIMER1_WGM_MODE,  WGM1, 1);
  TCCR1B =
    BITF(TIMER1_PRESCALER,  CS1, 0) |
    BITF(TIMER1_PRESCALER,  CS1, 1) |
    BITF(TIMER1_PRESCALER,  CS1, 2) |
    BITF(TIMER1_WGM_MODE,  WGM1, 2) |
    BITF(TIMER1_WGM_MODE,  WGM1, 3);

  /* output compare match A interrupt enable                      */
  TIMSK1 |= _BV(OCIE1A);
}


/** Configure 16bit timer to trigger an ISR four times as fast ast timer1_init() does.
 *
 * You MUST have run timer1_init() some time before running timer1_init_quick().
 */
void timer1_init_quick(void)
{
  const uint8_t old_tccr1b = TCCR1B;
  /* pause the clock */
  TCCR1B &= ~(_BV(CS12) | _BV(CS11) | _BV(CS10));
  /* faster blinking */
  OCR1A = TIMER1_COMPARE_MATCH_VAL / 4;
  /* start counting from 0, needs clock to be paused */
  TCNT1 = 0;
  /* unpause the clock */
  TCCR1B = old_tccr1b;
}


/** @} */

/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */

