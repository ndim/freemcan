/** \file freemcan-frame.h
 * \brief Data frame parser (layer 2) (interface)
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
 */


#ifndef FREEMCAN_FRAME_H
#define FREEMCAN_FRAME_H

#include <stdlib.h>

#include "frame-defs.h"


/** Data frame (parsed)
 *
 * In this parsed state, the header magic number and trailing checksum
 * have already been verified to be correct and thus thrown aside.
 */
typedef struct {
  /** Frame type */
  frame_type_t type;
  /** Payload size in bytes */
  uint16_t size;
  /** Payload */
  uint8_t payload[];
} frame_t;


/** Handler function for newly parsed frames
 * \param frame The frame to handle
 * \param data Private data for the handler function
 */
typedef void (*frame_handler_t)(const frame_t *frame);

/** Set frame handler function to the given function */
void frame_set_handler(frame_handler_t handler);

/** Reset frame handler function to No-Op */
void frame_reset_handler(void);


/** Parse a few bytes as frame
 *
 * Call this function repeatedly as the data is trickling in. Whenever
 * a complete frame is detected by the internal parsing logic, the
 * hander function the caller should have set by calling
 * #frame_set_handler will be called with the parsed frame.
 */
void frame_parse_bytes(const void *buf, const size_t size);


/** Reset checksum state machine */
void checksum_reset(void);

/** Update checksum state machine with value */
void checksum_update(const uint8_t value);

/** Write checksum to file descriptor */
void checksum_write(const int fd);


#endif /* !FREEMCAN_FRAME_H */
