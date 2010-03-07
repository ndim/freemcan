/** \file hostware/freemcan-export.h
 * \brief histogram export functions
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

#include "freemcan-packet.h"

/** external prototype for gnuplot-export function call */
void export_histogram(const packet_histogram_t *histogram_packet);

/** @} */

#endif /* !FREEMCAN_EXPORT_H */
