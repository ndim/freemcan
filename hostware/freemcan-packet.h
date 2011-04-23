/** \file hostware/freemcan-packet.h
 * \brief Data packets (layer 3) (interface)
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
 * \addtogroup freemcan_packets
 * @{
 */

#ifndef FREEMCAN_PACKET_H
#define FREEMCAN_PACKET_H

#include <time.h>

#include "packet-defs.h"

#include "packet-value-table.h"
#include "personality-info.h"


/** Callback function type called when value table packet arrives
 *
 * The callback function must call #packet_value_table_ref if it wants
 * to use the packet after returning, and then is responsible for
 * calling #packet_value_table_unref when it has finished accessing it.
 */
typedef void (*packet_handler_value_table_t)(packet_value_table_t *packet_value_table,
                                             void *data);

/** Callback function type called when state packet arrives */
typedef void (*packet_handler_state_t)(const char *state, void *data);


/** Callback function type called when text packet arrives */
typedef void (*packet_handler_text_t)(const char *text, void *data);


/** Callback function type called when text packet arrives */
typedef void (*packet_handler_params_from_eeprom_t)(const void *params,
                                                    const size_t size,
                                                    void *data);


/** Callback function type called when personality info packet arrives */
typedef void (*packet_handler_personality_info_t)(personality_info_t *pi,
                                                  void *data);

/** @} */

#endif /* !FREEMCAN_PACKET_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
