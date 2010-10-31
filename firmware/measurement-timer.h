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
extern volatile uint16_t timer_count;


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
extern volatile uint16_t last_timer_count;


/** Original timer count received in the command.
 *
 * Used later for determining how much time has elapsed yet. Written
 * once only, when the command has been received.
 */
extern volatile uint16_t orig_timer_count;


/** Initialize the 16bit timer */
void timer_init(const uint8_t timer0, const uint8_t timer1);


/** Make timer run more quickly */
void timer_init_quick(void);


/** \todo document this */
uint16_t get_duration(void);


/** @} */

#endif /* MEASUREMENT_TIMER_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
