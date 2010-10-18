/** \file firmware/measurement-timer.h
 * \brief Measurement timer
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
 * \addtogroup measurement_timer
 * @{
 */

#ifndef MEASUREMENT_TIMER_H
#define MEASUREMENT_TIMER_H


#include <stdint.h>

#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/pgmspace.h>

#include "global.h"
#include "packet-comm.h"
#include "wdt-softreset.h"


/** timer counter
 *
 * Initialized once by main() with value received from host
 * controller. Never touched by main() again after starting the timer
 * interrupt.
 *
 * Timer interrupt handler has exclusive access to read/writes
 * timer_count to decrement, once the timer ISR has been enabled.
 */
volatile uint16_t timer_count;


/** Last value of timer counter
 *
 * Used for pseudo synchronized reading of the timer_count multi-byte
 * variable in the main program, while timer_count may be written to
 * by the timer ISR.
 *
 * \see get_duration, ISR(TIMER1_COMPA_vect)
 *
 * The initial default value of last_timer_count was 1
 * originally. However, a last_timer_count value of 0 should not make
 * a difference, so we now rely on the implicit initialization to 0.
 */
volatile uint16_t last_timer_count;


/** Original timer count received in the command.
 *
 * Used later for determining how much time has elapsed yet. Written
 * once only, when the command has been received.
 */
volatile uint16_t orig_timer_count;


/** Configure 16 bit timer to trigger an ISR every second
 *
 * Configure "measurement in progress toggle LED-signal"
 */
inline static
void timer_init(const uint8_t timer0, const uint8_t timer1)
{
  /** Set up timer with the combined value we just got the bytes of.
   *
   * For some reasons, the following line triggers a bug with
   * the avr-gcc 4.4.2 and 4.5.0 we have available on Fedora
   * 12 and Fedora 13. Debian Lenny (5.05)'s avr-gcc 4.3.2
   * does not exhibit the buggy behaviour, BTW. So we do the
   * assignments manually here.
   *
   * orig_timer_count = (((uint16_t)timer1)<<8) | timer0;
   * timer_count = orig_timer_count;
   */
  asm("\n\t"
      "sts orig_timer_count,   %[timer0]\n\t"
      "sts orig_timer_count+1, %[timer1]\n\t"
      "sts timer_count,   %[timer0]\n\t"
      "sts timer_count+1, %[timer1]\n\t"
      : /* output operands */
      : /* input operands */
        [timer0] "r" (timer0),
        [timer1] "r" (timer1)
      );

  /** Safeguard: We cannot handle 0 or 1 count measurements. */
  if (orig_timer_count <= 1) {
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
  TCCR1B |=  ((((TIMER_PRESCALER >> 2) & 0x1)*_BV(CS12)) |
              (((TIMER_PRESCALER >> 1) & 0x1)*_BV(CS11)) |
              ((TIMER_PRESCALER & 0x01)*_BV(CS10)));

  /* Compare match value into output compare reg. A               */
  OCR1A = TIMER_COMPARE_MATCH_VAL;

  /* output compare match A interrupt enable                      */
  TIMSK1 |= _BV(OCIE1A);
}


/** Configure 16bit timer to trigger an ISR four times as fast ast timer_init() does.
 *
 * You MUST have run timer_init() some time before running timer_init_quick().
 */
inline static
void timer_init_quick(void)
{
  const uint8_t old_tccr1b = TCCR1B;
  /* pause the clock */
  TCCR1B &= ~(_BV(CS12) | _BV(CS11) | _BV(CS10));
  /* faster blinking */
  OCR1A = TIMER_COMPARE_MATCH_VAL / 4;
  /* start counting from 0, needs clock to be paused */
  TCNT1 = 0;
  /* unpause the clock */
  TCCR1B = old_tccr1b;
}


/** Get elapsed time in the currently running or finished measurement
 *
 * We do synchronized reading of the multi-byte variable timer_count
 * here (which is being written to by the ISR(TIMER1_COMPA_vect) while
 * send_table() is being executed for 'I' value tables). For all other
 * types of value tables, this busy sync loop will still work, but is
 * not required as all interrupts will be disabled.
 *
 * Reading both #timer_count and #last_timer_count consists basically of
 * the following four steps:
 *
 *   a) read lo8(#timer_count)
 *
 *   b) read hi8(#timer_count)
 *
 *   c) read lo8(#last_timer_count)
 *
 *   d) read hi8(#last_timer_count)
 *
 * Now we have a finite set of sequences in which those instructions
 * and ISR(TIMER1_COMPA_vect) can be executed in relation to each
 * other (keep in mind that the #timer_count is counted backwards):
 *
 *  1. ISR before a): No problem whatsoever.  #last_timer_count will
 *     be 1 more than #timer_count.  The while loop will finish.
 *
 *  2. ISR between a) and b): #timer_count will differ from
 *     #last_timer_count by two, or a lot more in the case of a 8bit
 *     overflow happening.  The while loop will continue.
 *
 *  3. ISR between b) and c): #timer_count will be the same as
 *     #last_timer_count.  The while loop will continue.
 *
 *  4. ISR between c) and d): #timer_count will be the same as
 *     #last_timer_count (just like case 3), or a lot more in the case
 *     of a 8bit overflow happening.  The while loop will continue.
 *
 *  5. ISR after d): No problem whatsoever.  #last_timer_count will be
 *     1 more than #timer_count.  The while loop will finish.
 *
 * As the ISR runs only every second, and we can reasonably presume
 * that the while loop can repeats a number of times during that
 * second, this will terminate quite quickly with a useful result.
 *
 * The result may be off by one for the 'I' value table packets but
 * not by more, and for 'I' results we can tolerate that kind of
 * inaccuracy.
 *
 * Durations for finished measurements will always be accurate, as
 * that will trigger case 1.
 *
 * \see last_timer_count, ISR(TIMER1_COMPA_vect)
 */
inline static
uint16_t get_duration(void)
{
  uint16_t a, b;
  do {
    a = timer_count;
    b = last_timer_count;
  } while ((b-a) != 1);
  /* Now 'a' contains a valid value. Use it. */
  const uint16_t duration = orig_timer_count - a;
  return duration;
}


/** @} */

#endif /* MEASUREMENT_TIMER_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
