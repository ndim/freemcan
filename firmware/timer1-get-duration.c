/** \file firmware/timer1-get-duration.c
 * \brief  Get duration measurement timer has been running
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
 * \defgroup timer1_get_duration Measurement Timer ISR: Get Duration
 * \ingroup firmware_personality_groups
 *
 * @{
 */

#include "timer1-get-duration.h"
#include "timer1-measurement.h"

/** Get elapsed time in the currently running or finished measurement
 *
 * We do "synchronized" reading of the multi-byte variable timer1_count
 * here (which is being written to by the ISR(TIMER1_COMPA_vect) while
 * send_table() is being executed for 'I' value tables). For all other
 * types of value tables, this busy sync loop will still work, but is
 * not required as all interrupts will be disabled.
 *
 * Reading #timer1_count and #last_timer1_count consists basically of
 * the following four steps:
 *
 *   a) read lo8(#timer1_count) for the first time
 *
 *   b) read hi8(#timer1_count) for the first time
 *
 *   c) read lo8(#timer1_count) for the second time
 *
 *   d) read hi8(#timer1_count) for the second time
 *
 * Now we have a finite set of sequences in which those instructions
 * and ISR(TIMER1_COMPA_vect) can be executed in relation to each
 * other:
 *
 *  1. ISR before a): No problem whatsoever.
 *     The two read #timer1_count values will be the same.
 *     The while loop will finish instantly.
 *
 *  2. ISR between a) and b): The #timer1_count values will differ.
 *     The while loop will continue.
 *
 *  3. ISR between b) and c): The #timer1_count values will differ.
 *     The while loop will continue.
 *
 *  4. ISR between c) and d): The #timer1_count values will differ.
 *     The while loop will continue.
 *
 *  5. ISR after d): No problem whatsoever.
 *     The two read #timer1_count values will be the same.
 *     The while loop will finish instantly.
 *
 * As the ISR runs only every second, and we can reasonably presume
 * that the while loop can repeat a number of times during that
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
 * \see timer1_count, ISR(TIMER1_COMPA_vect)
 */
uint16_t get_duration(void)
{
  uint16_t a, b;
  a = timer1_count;
  do {
    b = a;
    a = timer1_count;
  } while (a != b);
  /* Now 'a' and 'b' both contain the same valid #timer1_count value. */
  const uint16_t duration = orig_timer1_count - a;
  return duration;
}


/** @} */

/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
