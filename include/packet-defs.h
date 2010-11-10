/** \file include/packet-defs.h
 * \brief Data packet definitions (layer 3)
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
 * \defgroup packet_defs Packet Format
 * \ingroup communication_protocol
 * @{
 *
 * \section packet_host_to_emb Packets sent from hostware to firmware
 *
 * The host never sends packets to the firmware. It only sends frames.
 * See the \ref frame_defs "frame documentation".
 *
 * \section packet_emb_to_host From firmware to hostware: Value table packet
 *
 * The size of the value table data is determined from the total
 * packet data size (i.e. the frame's payload size) by subtracting the
 * size of the #packet_value_table_header_t header that is sent in
 * front of the actual value table data, and then subtracting the
 * param_buf_length as taken from the header.
 *
 * <table class="table header-top">
 *  <tr><th>size in bytes</th> <th>name</th> <th>C type define</th> <th>description</th></tr>
 *  <tr><td>sizeof(packet_value_table_header_t)</td> <td>header</td> <td>packet_value_table_header_t</td> <td>value table packet header</td></tr>
 *  <tr><td><em>header.param_buf_length</em></td> <td>param_buf</td> <td>uint8_t []</td> <td>firmware sends back the same parameter buffer that started the measurement</td></tr>
 *  <tr><td><em>see text</em></td> <td>data_table</td> <td>uintX_t []</td> <td>value table data</td></tr>
 * </table>
 *
 * \section packet_emb_to_host From firmware to hostware: Personality Information packet
 *
 * The personality information packet just contains a single instance
 * of the #packet_personality_info_t data structure.
 *
 */

#ifndef PACKET_DEFS_H
#define PACKET_DEFS_H


#include <stdint.h>

#include "compiler.h"



/** Type of value table
 *
 * The kind of data in the value table.
 */
typedef enum {

  /** Histogram data */
  VALUE_TABLE_TYPE_HISTOGRAM = 'H',

  /** Time series (e.g. repeated geiger counter) data */
  VALUE_TABLE_TYPE_TIME_SERIES = 'T',

  /** Timed samples */
  VALUE_TABLE_TYPE_SAMPLES = 'S'
} packet_value_table_type_t;


/** Value table packet reason for sending
 *
 * The reason for sending the value table.
 */
typedef enum {

  /** Regular intermediate report. */
  PACKET_VALUE_TABLE_INTERMEDIATE = 'I',

  /** Measurement has completed ("done"). */
  PACKET_VALUE_TABLE_DONE = 'D',

  /** Repeat sending of 'D' type value table */
  PACKET_VALUE_TABLE_RESEND = 'R',

  /** Measurement has been aborted, report results as gathered so far. */
  PACKET_VALUE_TABLE_ABORTED = 'A'

} packet_value_table_reason_t;


/** Maximum length of parameter block in bytes */
#define MAX_PARAM_LENGTH 16


/** Value table packet header
 *
 * \todo Verify the compiler does not do strange alignment things.
 *
 * Note: If you change this structure, please make sure you update the
 * table above.
 *
 * Note 2: This struct is quite sensible to compiler settings,
 * alignment, packing and order of the members. If this turns out to
 * be too fragile, we need to get rid of the struct, and read the
 * values byte-by-byte by hand. The current struct works with
 *
 *   * avr-gcc-4.5.1 on AVR
 *   * native gcc-4.5.1 on i386
 */
typedef struct {
  /** value table element size in bytes (1,2,3,4) */
  uint8_t  element_size;
  /** Reason for sending value table (#packet_value_table_reason_t cast to uint8_t) */
  uint8_t  reason;
  /** Type of value table (#packet_value_table_type_t cast to uint8_t) */
  uint8_t  type;
  /** duration of measurement that lead to the attached data */
  uint16_t duration;
  /** length of the token (a number of bytes sent back unchanged) */
  uint8_t param_buf_length;
} PACKED packet_value_table_header_t;


/** Personality Information packet content */
typedef struct {
  /** Maximum size of the complete table in byte */
  uint16_t sizeof_table;
  /** Size of a single table value in byte */
  uint16_t sizeof_value;
  /** Time units per second */
  uint8_t units_per_second;
  /** Size of measurement command's parameter elements */
  uint8_t param_data_size_timer_count;
  uint8_t param_data_size_skip_samples;
} PACKED packet_personality_info_t;



/** @} */

#endif /* !PACKET_DEFS_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
