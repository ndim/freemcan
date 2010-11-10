/** \file firmware/main.h
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
 * \ingroup firmware_generic
 *
 * @{
 */

#ifndef MAIN_H
#define MAIN_H


#include <stdint.h>

#include "packet-defs.h"


/** Firmware internal store for the parameter buffer */
typedef struct {
  uint8_t length;
  uint8_t params[MAX_PARAM_LENGTH];
} personality_param_t;


extern personality_param_t pparam_sram;


/** Notification function called when measurement has finished.
 *
 * This is the place e.g. to display to the user that the measurement
 * has finished.
 */
void on_measurement_finished(void);


void personality_start_measurement_sram(void);
void personality_start_measurement_eeprom(void);


#endif /* MAIN_H */

/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
