/** \file freemcan-frame.h
 * \brief Data frame parser (interface)
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

typedef struct {
  frame_type_t type;
  uint16_t size;
  uint8_t payload[];
} frame_t;

/** Handle frame
 * \param frame The frame to handle
 * \param data Private data for the handler function
 */
typedef void (*frame_handler_t)(const frame_t *frame,
				void *data);

void frame_set_handler(frame_handler_t handler, void *data);
void frame_reset_handler(void);

void frame_parse(const void *buf, const size_t size);

#endif /* !FREEMCAN_FRAME_H */
