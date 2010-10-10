/** \file firmware/measurement-timer-ISR-countdown-and-stop.c
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
 * \defgroup measurement_timer_ISR_countdown Measurement Timer ISR: Count down to zero then stop
 * \ingroup firmware
 *
 * Measurement timer ISR: Count down to zero then stop
 *
 * @{
 */

#include "measurement-timer.h"
#include "main.h"


/** 16 Bit timer ISR
 *
 * When timer has elapsed, the global #timer_flag (8bit, therefore
 * atomic read/writes) is set.
 *
 * Note that we are counting down the timer_count, so it will start
 * with its maximum value and count down to zero.
 *
 * \see last_timer_count, get_duration
 */
ISR(TIMER1_COMPA_vect)
{
  /* toggle a sign PORTD ^= _BV(PD5); (done automatically) */

  if (!measurement_finished) {
    /** We do not touch #measurement_finished ever again after setting
     * it. */
    last_timer_count = timer_count;
    timer_count--;
    if (timer_count == 0) {
      /* timer has elapsed, set the flag to signal the main program */
      measurement_finished = 1;
    }
  }
}


void on_measurement_finished(void)
{
  timer_init_quick();
}


/** @} */

/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
