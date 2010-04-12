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

#include "freemcan-packet.h"


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
			 packet_handler_state_t state_packet_handler,
			 packet_handler_text_t text_packet_handler,
			 void *data);

/** @} */

#endif /* !FREEMCAN_PACKET_PARSER_H */
