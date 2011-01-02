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
volatile uint16_t orig_timer1_count;


/** 16 Bit timer ISR
 *
 * When timer has elapsed, the global #timer1_flag (8bit, therefore
 * atomic read/writes) is set.
 *
 * Note that we are counting down the #timer1_count, so it will start
 * with its maximum value and count down to zero.
 *
 * \see timer1_count, get_duration
 */
ISR(TIMER1_COMPA_vect)
{
  /* toggle a sign PORTD ^= _BV(PD5); (done automatically) */

  if (GF_IS_CLEARED(GF_MEASUREMENT_FINISHED)) {
    /** We do not touch #measurement_finished ever again after setting
     * it. */
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
