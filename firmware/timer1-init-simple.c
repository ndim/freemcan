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
 * \ingroup firmware
 *
 * Timer init to simply periodically trigger timer ISR.
 *
 * @{
 */


#include "data-table.h"
#include "timer1-constants.h"
#include "timer1-measurement.h"


/** Configure 16 bit timer to trigger an ISR every second
 *
 * Configure "measurement in progress toggle LED-signal"
 */
void timer1_init(const uint16_t timer1_value)
{
  orig_timer1_count = timer1_count = timer1_value;

  /** Safeguard: We cannot handle 0 or 1 count measurements. */
  if (orig_timer1_count <= 1) {
    send_text_P(PSTR("Unsupported timer value <= 1"));
    wdt_soft_reset();
  }

  /* Prepare timer 0 control register A and B for
     clear timer on compare match (CTC)                           */
  TCCR1A = 0;
  TCCR1B =  _BV(WGM12);

  /* Configure "measurement in progress LED"                      */
  /* configure pin 19 as an output */
  DDRD |= (_BV(DDD5));
  /* toggle LED pin 19 on compare match automatically             */
  TCCR1A |= _BV(COM1A0);

  /* Prescaler settings on timer conrtrol reg. B                  */
  TCCR1B |=  ((((TIMER1_PRESCALER >> 2) & 0x1)*_BV(CS12)) |
              (((TIMER1_PRESCALER >> 1) & 0x1)*_BV(CS11)) |
              ((TIMER1_PRESCALER & 0x01)*_BV(CS10)));

  /* Compare match value into output compare reg. A               */
  OCR1A = TIMER1_COMPARE_MATCH_VAL;

  /* output compare match A interrupt enable                      */
  TIMSK1 |= _BV(OCIE1A);
}


void personality_start_measurement_sram(void)
{
  const void *voidp = &personality_param_sram[0];
  const uint16_t *timer1_value = voidp;
  timer1_init(*timer1_value);
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

