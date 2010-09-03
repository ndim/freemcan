/** \file hostware/frame-parser.h
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
 *
 * \addtogroup freemcan_frame_parser
 * @{
 */


#ifndef FREEMCAN_FRAME_PARSER_H
#define FREEMCAN_FRAME_PARSER_H

#include <stdbool.h>
#include <stdlib.h>


/** frame parser (opaque data type) */
struct _frame_parser_t;

/** frame parser (opaque data type) */
typedef struct _frame_parser_t frame_parser_t;


#include "packet-parser.h"


frame_parser_t *frame_parser_new(packet_parser_t *packet_parser)
  __attribute__(( malloc ))
  __attribute__(( nonnull(1) ));


void frame_parser_ref(frame_parser_t *self)
  __attribute__(( nonnull(1) ));


void frame_parser_unref(frame_parser_t *self)
  __attribute__(( nonnull(1) ));


/** Parse a few bytes as frame
 *
 * Call this function repeatedly as the data is trickling in. Whenever
 * a complete frame is detected by the internal parsing logic, the
 * self's packet_parser will be notified via #packet_parser_handle_frame.
 */
void frame_parser_bytes(frame_parser_t *self,
                        const void *buf, const size_t size)
  __attribute__(( nonnull(1,2) ));


/** Whether to dump layer 1 data (byte stream) into log */
extern bool enable_layer1_dump;


/** Whether to dump layer 2 data (frames) into log */
extern bool enable_layer2_dump;


/** @} */

#endif /* !FREEMCAN_FRAME_PARSER_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
