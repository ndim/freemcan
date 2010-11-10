/** \file firmware/data-table.h
 * \brief Data table interface between MCA/time series and send_histogram()
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
 * \defgroup data_table Data table interface between MCA/time series and send_table()
 * \ingroup firmware_generic
 *
 * @{
 */

#ifndef DATA_TABLE_H
#define DATA_TABLE_H


#include <stdlib.h>
#include <avr/eeprom.h>
#include <avr/pgmspace.h>

#include "packet-defs.h"


/** Declare all personality information
 *
 * \param NAME Personality name as string, e.g. "adc-int-mca"
 * \param PARAM_SIZE_TIMER1_COUNT Size of timer1_count param in bytes (0 or 2)
 * \param PARAM_SIZE_SKIP_SAMPLES Size of skip_samples param in bytes (0 or 2)
 * \param UNITS_PER_SECOND Timer units per second, e.g. 1 (for 1sec timer period
 *                         or 10 (for 0.1sec timer period).
 * \param MAX_BYTES_PER_TABLE Maximum size of data table in bytes
 *                            (in bytes to make use of compile time constants)
 * \param TABLE_ELEMENT_SIZE Size of a single element in the data table in bytes
 */
#define PERSONALITY(NAME,                                           \
                    PARAM_SIZE_TIMER1_COUNT,                        \
                    PARAM_SIZE_SKIP_SAMPLES,                        \
                    UNITS_PER_SECOND,                               \
                    MAX_BYTES_PER_TABLE,                            \
                    TABLE_ELEMENT_SIZE)                             \
  const packet_personality_info_t personality_info PROGMEM = {      \
    MAX_BYTES_PER_TABLE,                                            \
    TABLE_ELEMENT_SIZE,                                             \
    UNITS_PER_SECOND,                                               \
    PARAM_SIZE_TIMER1_COUNT,                                        \
    PARAM_SIZE_SKIP_SAMPLES                                         \
  };                                                                \
  const char personality_name[] PROGMEM = NAME;                     \
  const uint8_t personality_name_length = sizeof(NAME)-1;           \
  const uint8_t personality_param_size = (PARAM_SIZE_TIMER1_COUNT+PARAM_SIZE_SKIP_SAMPLES)

extern const char personality_name[] PROGMEM;
extern const uint8_t personality_name_length;

extern const uint8_t personality_param_size;
extern uint8_t personality_param_sram[];
extern uint8_t personality_param_eeprom[] EEMEM;

extern const packet_personality_info_t personality_info PROGMEM;


/** The data table as an opaque array of bytes
 *
 * The actual structure of the bytes in the table is undefined for the
 * purpose of this interface.
 */
extern char data_table[];


/** Information about the data table */
typedef struct {
  /** The size of the #data_table in bytes */
  size_t size;

  /** The type of value table */
  packet_value_table_type_t type;

  /** The size of a single table element */
  uint8_t element_size;
} data_table_info_t;


/** Information about the data table
 *
 * Set by the personality specific code, and read by send_table().
 */
extern data_table_info_t data_table_info;


/** @} */

#endif /* DATA_TABLE_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
