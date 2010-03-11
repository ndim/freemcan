/** \file hostware/freemcan-packet.h
 * \brief Data packet parser (layer 3) (interface)
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
 * \addtogroup freemcan_packet
 * @{
 */

#ifndef FREEMCAN_PACKET_H
#define FREEMCAN_PACKET_H

#include <time.h>

#include "packet-defs.h"


/** Parsed histogram packet. */
typedef struct {

  /** The reason for sending the histogram */
  packet_histogram_type_t type;

  /** Size of a single histogram element in bytes (1,2,4,8) */
  uint8_t element_size;

  /** Timestamp when package was received */
  time_t receive_time;

  /** Number of elements in histogram array */
  size_t element_count;

  /** Histogram element table.
   *
   * This is a single pointer to the actual memory area with the
   * histogram data, accessible as multiple types via a union for your
   * convenience.
   */
  union {
    const uint8_t  *e8;
    const uint16_t *e16;
    const uint32_t *e32;
    const uint64_t *e64;
    const void     *ev;
  } elements;
} packet_histogram_t;


/** Callback function type called when histogram packet arrives */
typedef void (*packet_handler_histogram_t)(const packet_histogram_t *packet_histogram,
					   void *data);

/** Callback function type called when status packet arrives */
typedef void (*packet_handler_status_t)(const char *status, void *data);

/** Callback function type called when text packet arrives */
typedef void (*packet_handler_text_t)(const char *status, void *data);


/** Reset packet handler callbacks.
 *
 * This also unregisters the packet parser callback from the frame
 * parser.
 */
void packet_reset_handlers();


/** Set the packet handler callbacks.
 *
 * This also registers the packet parser callback with the frame
 * parser.
 */
void packet_set_handlers(packet_handler_histogram_t histogram_packet_handler,
			 packet_handler_status_t status_packet_handler,
			 packet_handler_text_t text_packet_handler,
			 void *data);

/** @} */

#endif /* !FREEMCAN_PACKET_H */
