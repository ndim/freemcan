/** \file firmware/main.c
 * \brief API to firmware main loop
 *
 * \author Copyright (C) 2009 samplemaker
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
 * \defgroup firmware_main Firmware Main
 * \ingroup firmware
 *
 * @{
 */

#ifndef MAIN_H
#define MAIN_H


#include <stdint.h>


/** Flag to signal that the measurement has finished
 *
 * Used to signal from the measurement ISRs to the main program that
 * the measurement has finished.
 *
 * It is 0 by default, and will be set to non-zero as true. Written
 * once only by the measurement ISRs, and read by the main loop.
 *
 * It is an 8bit value, and thus accessible with atomic read/write
 * operations.
 */
extern volatile uint8_t measurement_finished;


/** Notification function called when measurement has finished.
 *
 * This is the place e.g. to display to the user that the measurement
 * has finished.
 */
void on_measurement_finished(void);


#endif /* MAIN_H */

/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
