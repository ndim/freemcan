/** \file hostware/freemcan-export.h
 * \brief Value table export functions
 *
 * \author Copyright (C) 2010 samplemaker
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
 * \addtogroup freemcan_export
 * @{
 */


#ifndef FREEMCAN_EXPORT_H
#define FREEMCAN_EXPORT_H

#include <stdbool.h>

#include "freemcan-packet.h"


/** Flag that next 'I' packet is to be written
 *
 * Hackish, but works - as follows:
 *
 * The main loop requests intermediate values either with
 * write_next_intermediate_packet set or cleared. If set,
 * export_value_table() will clear write_next_intermediate_packet and
 * write the packet to a file. If write_next_intermediate_packet is
 * cleared, export_value_table() will not write intermediate values to
 * a file.
 */
bool write_next_intermediate_packet;


/** \brief Write the given value table to a newly created file
 * \ingroup freemcan_export
 *
 * The name of the newly created file is created based on the current
 * local time of day. Given the rate at which we can receive new
 * value tables is much less than one per second, this should avoid file
 * name collisions.
 *
 * If a file of the same name happens to already exist, it will be
 * overwritten.
 *
 * You can plot the most recent histogram with the helper utility
 * "pltHist.pl" from this very directory.
 */
void export_value_table(const personality_info_t *personality_info,
                        const packet_value_table_t *value_table_packet);


/** Compute default file name for exporting given value packet packet data to.
 *
 * \return The return value points to a global static buffer.
 */
char *export_value_table_get_filename(const packet_value_table_t *value_table_packet,
                                      const char *extension);

/** @} */

#endif /* !FREEMCAN_EXPORT_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
