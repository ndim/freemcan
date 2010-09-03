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
 * The host never sends packets to the firmware. It only sends
 * simplified frames.  See the \ref frame_defs "frame documentation".
 *
 * \section packet_emb_to_host From firmware to hostware: Histogram packet
 *
 * The size of the histogram data is determined from the total packet
 * data size (i.e. the frame's payload size) by subtracting the size
 * of the #packet_histogram_header_t that is sent in front of
 * histogram data.
 *
 * <table class="table header-top">
 *  <tr><th>size in bytes</th> <th>C type define</th> <th>description</th></tr>
 *  <tr><td>sizeof(packet_histogram_header_t)</td> <td>packet_histogram_header_t</td> <td>histogram packet header</td></tr>
 *  <tr><td>see above</td> <td>uintX_t []</td> <td>histogram data</td></tr>
 * </table>
 *
 */

#ifndef PACKET_DEFS_H
#define PACKET_DEFS_H


#include <stdint.h>



/** Histogram packet types (UNUSED SO FAR)
 *
 * The reason for sending the histogram. (UNUSED SO FAR)
 */
typedef enum {

  /** Regular intermediate report. */
  PACKET_HISTOGRAM_INTERMEDIATE = 'I',

  /** Measurement has completed ("done"). */
  PACKET_HISTOGRAM_DONE = 'D',

  /** Repeat sending of 'D' type histogram */
  PACKET_HISTOGRAM_RESEND = 'R',

  /** Measurement has been aborted, report results as gathered so far. */
  PACKET_HISTOGRAM_ABORTED = 'A'

} packet_histogram_type_t;


/** Histogram packet header
 *
 * \todo Verify the compiler does not do strange alignment things.
 *
 * Note: If you change this structure, please make sure you update the
 * table above.
 */
typedef struct {
  /** histogram element size in bytes (1,2,3,4) */
  uint8_t  element_size;
  /** histogram type (#packet_histogram_type_t cast to uint8_t) */
  uint8_t  type;
  /** duration of measurement that lead to the attached data */
  uint16_t duration;
  /** total duration (of the measurement in progress) */
  uint16_t total_duration;
} packet_histogram_header_t;


/** @} */

#endif /* !PACKET_DEFS_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
