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


/** Parsed value table packet. */
typedef struct {
  /** Reference counter */
  int refs;

  /** The reason for sending the value table */
  packet_value_table_reason_t reason;

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

  /** Maximum "good" value from elements[] array (ignores clamping value!) */
  uint32_t max_value;

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
                                             const time_t receive_time,
                                             const uint8_t element_size,
                                             const size_t element_count,
                                             const uint16_t duration,
                                             const uint16_t total_duration,
                                             const void *elements)
  __attribute__((malloc));


/** Call this when you want to use value_table and store a pointer to it. */
void packet_value_table_ref(packet_value_table_t *value_table);


/** Call this when you have finished using your pointer to value_table. */
void packet_value_table_unref(packet_value_table_t *value_table);


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


/** @} */

#endif /* !FREEMCAN_PACKET_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
