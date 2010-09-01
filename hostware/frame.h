/** \file hostware/frame.h
 * \brief Data frame (interface)
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
 * \addtogroup freemcan_frame
 * @{
 */


#ifndef FREEMCAN_FRAME_H
#define FREEMCAN_FRAME_H

#include <stdbool.h>
#include <stdlib.h>

#include "frame-defs.h"


/** Data frame (parsed)
 *
 * In this parsed state, the header magic number and trailing checksum
 * have already been verified to be correct and thus thrown aside.
 */
typedef struct {
  /** Reference counter */
  int refs;
  /** Frame type */
  frame_type_t type;
  /** Payload size in bytes */
  uint16_t size;
  /** Payload */
  uint8_t payload[];
} frame_t;


frame_t *frame_new(const size_t payload_size)
  __attribute__((malloc));


void frame_ref(frame_t *self)
  __attribute__(( nonnull(1) ));


void frame_unref(frame_t *self)
  __attribute__(( nonnull(1) ));


/** @} */

#endif /* !FREEMCAN_FRAME_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
