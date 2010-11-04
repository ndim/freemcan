/** \file hostware/freemcan-packet.h
 * \brief Data packets (layer 3) (interface)
 *
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
 * \addtogroup freemcan_packets
 * @{
 */

#ifndef FREEMCAN_PACKET_H
#define FREEMCAN_PACKET_H

#include <time.h>

#include "packet-defs.h"


/** Parsed personality info */
typedef struct {
  int refs;
  size_t sizeof_table;
  size_t sizeof_value;
  unsigned int units_per_second;
  size_t param_data_size;
  char personality_name[];
} personality_info_t;


/** Parsed value table packet. */
typedef struct {
  /** Reference counter */
  int refs;

  /** The reason for sending the value table */
  packet_value_table_reason_t reason;

  /** The type of value table */
  packet_value_table_type_t type;

  /** Timestamp when package was received */
  time_t receive_time;

  /** Number of elements in value table array */
  size_t element_count;

  /** Size of each received value table element in bytes */
  size_t orig_element_size;

  /** Duration of measurement which lead to the value table data, or
   * time spent recording the last item in the time series. */
  unsigned int duration;

  /** Total scheduled duration of the measurement in progress, or the
   * time spent recording all but the last item in the time series. */
  unsigned int total_duration;

  /** Token (value sent back unchanged) */
  uint32_t token;

  /** Value table array (native endian uint32_t) */
  uint32_t elements[];
} packet_value_table_t;


/** Create (allocate and initialize) a new packet_value_table_t instance.
 *
 * \param type Reason for sending the value table packet
 * \param receive_time Timestamp at which the packet was received.
 * \param element_size Size of each element in bytes (1,2,3,4).
 * \param element_count The number of elements received from device.
 * \param duration The duration of the measurement which produced
 *                 the data in elements.
 * \param total_duration The total duration of the running measurement
 *                       until it will be completed.
 * \param elements Pointer to the array of data as received from the
 *                 device.  A NULL pointer is interpreted like an
 *                 array consisting entirely of zeros.
 *
 * Note that the determination of max_value disregards the last value
 * in the array due to that being the value where ADC clamping is
 * counted.
 */
packet_value_table_t *packet_value_table_new(const packet_value_table_reason_t reason,
                                             const packet_value_table_type_t type,
                                             const time_t receive_time,
                                             const uint8_t element_size,
                                             const size_t element_count,
                                             const uint16_t _duration,
                                             const uint16_t _total_duration,
                                             const uint32_t _token,
                                             const void *elements)
  __attribute__((malloc));


/** Call this when you want to use value_table and store a pointer to it. */
void packet_value_table_ref(packet_value_table_t *value_table);


/** Call this when you have finished using your pointer to value_table. */
void packet_value_table_unref(packet_value_table_t *value_table);


/** Create (allocate and initialize) a new personality_info_t instance.
 */
personality_info_t *personality_info_new(const uint16_t _sizeof_table,
                                         const uint16_t _sizeof_value,
                                         const uint8_t units_per_second,
                                         const uint8_t param_data_size,
                                         const uint16_t _personality_name_size,
                                         const char *personality_name)
  __attribute__((malloc));


/** Call this when you want to use value_table and store a pointer to it. */
void personality_info_ref(personality_info_t *pi);


/** Call this when you have finished using your pointer to value_table. */
void personality_info_unref(personality_info_t *pi);


/** Callback function type called when value table packet arrives
 *
 * The callback function must call #packet_value_table_ref if it wants
 * to use the packet after returning, and then is responsible for
 * calling #packet_value_table_unref when it has finished accessing it.
 */
typedef void (*packet_handler_value_table_t)(packet_value_table_t *packet_value_table,
                                             void *data);

/** Callback function type called when state packet arrives */
typedef void (*packet_handler_state_t)(const char *state, void *data);


/** Callback function type called when text packet arrives */
typedef void (*packet_handler_text_t)(const char *text, void *data);


/** Callback function type called when text packet arrives */
typedef void (*packet_handler_params_from_eeprom_t)(const void *params,
                                                    const size_t size,
                                                    void *data);


/** Callback function type called when personality info packet arrives */
typedef void (*packet_handler_personality_info_t)(personality_info_t *pi,
                                                  void *data);

/** @} */

#endif /* !FREEMCAN_PACKET_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
