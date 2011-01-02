/** \file firmware/timer1-measurement.h
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
 * \defgroup timer1_measurement Timer1 as measurement timer
 * \ingroup firmware_generic
 *
 * @{
 */

#ifndef TIMER1_MEASUREMENT_H
#define TIMER1_MEASUREMENT_H


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
 * timer1_count to decrement, once the timer ISR has been enabled.
 */
extern volatile uint16_t timer1_count;


/** Original timer count received in the command.
 *
 * Used later for determining how much time has elapsed yet. Written
 * once only, when the command has been received.
 */
extern volatile uint16_t orig_timer1_count;


/** Initialize the 16bit timer */
void timer1_init(const uint16_t timer1_value);


/** Make timer run more quickly */
void timer1_init_quick(void);


/** @} */

#endif /* TIMER1_MEASUREMENT_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
