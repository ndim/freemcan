/** \file firmware/timer1-countdown-and-stop.c
 * \brief Measurement timer ISR: Count down to zero then stop
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
 * \defgroup timer1_countdown Measurement Timer ISR: Count down to zero then stop
 * \ingroup firmware_personality_groups
 *
 * Measurement timer ISR: Count down to zero then stop
 *
 * @{
 */

#include "timer1-measurement.h"
#include "main.h"


volatile uint16_t timer1_count;
volatile uint16_t last_timer1_count;
volatile uint16_t orig_timer1_count;


/** Get elapsed time in the currently running or finished measurement
 *
 * We do synchronized reading of the multi-byte variable timer1_count
 * here (which is being written to by the ISR(TIMER1_COMPA_vect) while
 * send_table() is being executed for 'I' value tables). For all other
 * types of value tables, this busy sync loop will still work, but is
 * not required as all interrupts will be disabled.
 *
 * Reading both #timer1_count and #last_timer1_count consists basically of
 * the following four steps:
 *
 *   a) read lo8(#timer1_count)
 *
 *   b) read hi8(#timer1_count)
 *
 *   c) read lo8(#last_timer1_count)
 *
 *   d) read hi8(#last_timer1_count)
 *
 * Now we have a finite set of sequences in which those instructions
 * and ISR(TIMER1_COMPA_vect) can be executed in relation to each
 * other (keep in mind that the #timer1_count is counted backwards):
 *
 *  1. ISR before a): No problem whatsoever.  #last_timer1_count will
 *     be 1 more than #timer1_count.  The while loop will finish.
 *
 *  2. ISR between a) and b): #timer1_count will differ from
 *     #last_timer1_count by two, or a lot more in the case of a 8bit
 *     overflow happening.  The while loop will continue.
 *
 *  3. ISR between b) and c): #timer1_count will be the same as
 *     #last_timer1_count.  The while loop will continue.
 *
 *  4. ISR between c) and d): #timer1_count will be the same as
 *     #last_timer1_count (just like case 3), or a lot more in the case
 *     of a 8bit overflow happening.  The while loop will continue.
 *
 *  5. ISR after d): No problem whatsoever.  #last_timer1_count will be
 *     1 more than #timer1_count.  The while loop will finish.
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
 * For now, we have placed this function right beside the timer ISR
 * which needs to properly update the values for get_duration() to
 * work.
 *
 * \see last_timer1_count, ISR(TIMER1_COMPA_vect)
 */
uint16_t get_duration(void)
{
  uint16_t a, b;
  do {
    a = timer1_count;
    b = last_timer1_count;
  } while ((b-a) != 1);
  /* Now 'a' contains a valid value. Use it. */
  const uint16_t duration = orig_timer1_count - a;
  return duration;
}


/** 16 Bit timer ISR
 *
 * When timer has elapsed, the global #timer1_flag (8bit, therefore
 * atomic read/writes) is set.
 *
 * Note that we are counting down the timer1_count, so it will start
 * with its maximum value and count down to zero.
 *
 * \see last_timer1_count, get_duration
 */
ISR(TIMER1_COMPA_vect)
{
  /* toggle a sign PORTD ^= _BV(PD5); (done automatically) */

  if (GF_IS_CLEARED(GF_MEASUREMENT_FINISHED)) {
    /** We do not touch #measurement_finished ever again after setting
     * it. */
    last_timer1_count = timer1_count;
    timer1_count--;
    if (timer1_count == 0) {
      /* timer has elapsed, set the flag to signal the main program */
      GF_SET(GF_MEASUREMENT_FINISHED);
    }
  }
}


void on_measurement_finished(void)
{
  timer1_init_quick();
}


/** @} */

/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
