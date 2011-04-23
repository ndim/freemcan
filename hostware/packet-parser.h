/** \file hostware/packet-parser.h
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
 * \addtogroup freemcan_packet_parser
 * @{
 */

#ifndef FREEMCAN_PACKET_PARSER_H
#define FREEMCAN_PACKET_PARSER_H

#include "packet-defs.h"

/** packet parser (opaque data type) */
struct _packet_parser_t;

/** packet parser (opaque data type) */
typedef struct _packet_parser_t packet_parser_t;


#include "freemcan-packet.h"


packet_parser_t *packet_parser_new(packet_handler_value_table_t value_table_packet_handler,
                                   packet_handler_state_t state_packet_handler,
                                   packet_handler_text_t text_packet_handler,
                                   packet_handler_personality_info_t packet_handler_personality_info,
                                   packet_handler_params_from_eeprom_t ph_params_from_eeprom,
                                   void *data)
  __attribute__(( warn_unused_result ))
  __attribute__(( malloc ));


void packet_parser_ref(packet_parser_t *self)
  __attribute__(( nonnull(1) ));


void packet_parser_unref(packet_parser_t *self)
  __attribute__(( nonnull(1) ));


#include "frame.h"

void packet_parser_handle_frame(packet_parser_t *self, const frame_t *frame)
  __attribute__(( nonnull(1,2) ));

/** Reset packet handler callbacks.
 *
 * This also unregisters the packet parser callback from the frame
 * parser.
 */
void packet_parser_reset_handlers(packet_parser_t *self)
  __attribute__(( nonnull(1) ));


/** @} */

#endif /* !FREEMCAN_PACKET_PARSER_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
