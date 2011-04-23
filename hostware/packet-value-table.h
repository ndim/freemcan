/** \file hostware/packet-value-table.h
 * \brief Value Table data packets (layer 3) (interface)
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
 * \addtogroup freemcan_packets_value_table
 * @{
 */

#ifndef FREEMCAN_PACKET_VALUE_TABLE_H
#define FREEMCAN_PACKET_VALUE_TABLE_H

#include <time.h>

#include "packet-defs.h"


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
   * time spent recording all but the last item in the time
   * series. "-1" if undefined. */
  unsigned int total_duration;

  /** Skip samples value. "-1" if undefined. */
  unsigned int skip_samples;

  /** Token bytes (value sent back unchanged) */
  char *token;

  /** Value table array (native endian uint32_t) */
  uint32_t elements[];
} packet_value_table_t;


/** Create (allocate and initialize) a new packet_value_table_t instance.
 *
 * \param reason Reason for sending the value table packet
 * \param type Type of value table
 * \param receive_time Timestamp at which the packet was received.
 * \param element_size Size of each element in bytes (1,2,3,4).
 * \param element_count The number of elements received from device.
 * \param _duration The duration of the measurement which produced
 *                  the data in elements.
 * \param param_buf_length Length of parameter buffer in bytes.
 * \param data Pointer to the remaining memory as received from the
 *             device. The memory contains first the parameter buffer
 *             followed by the actual value table.
 *             A NULL pointer is interpreted like an
 *             array consisting entirely of zeros.
 *
 * Note that the determination of max_value disregards the last value
 * in the array due to that being the value where ADC clamping is
 * counted.
 *
 * Note that the parameters starting with an underscore are in device
 * endianness.
 */
packet_value_table_t *packet_value_table_new(const packet_value_table_reason_t reason,
                                             const packet_value_table_type_t type,
                                             const time_t receive_time,
                                             const uint8_t element_size,
                                             const size_t element_count,
                                             const uint16_t _duration,
                                             const uint8_t param_buf_length,
                                             const void *data)
  __attribute__((warn_unused_result))
  __attribute__((malloc));


/** Call this when you want to use value_table and store a pointer to it. */
void packet_value_table_ref(packet_value_table_t *value_table)
  __attribute__((nonnull(1)));


/** Call this when you have finished using your pointer to value_table. */
void packet_value_table_unref(packet_value_table_t *value_table)
  __attribute__((nonnull(1)));


/** @} */

#endif /* !FREEMCAN_PACKET_VALUE_TABLE_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
