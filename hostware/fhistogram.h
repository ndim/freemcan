/** \file hostware/fhistogram.h
 * \brief Histogram Class (interface)
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
 */


#ifndef FHISTOGRAM_H
#define FHISTOGRAM_H

#include "freemcan-packet.h"

/**
 * \addtogroup fhistogram
 *
 * \bug Now with the new and updated packet_histogram_t, fhistogram_t
 *      does not offer anything over what packet_histogram_t already
 *      provides.
 *
 * @{
 */

typedef struct {
  int refs;
  packet_histogram_t *packet;
} fhistogram_t;

fhistogram_t *fhistogram_new_zero(const size_t elements)
  __attribute__((warn_unused_result))
  __attribute__((malloc));

fhistogram_t *fhistogram_new_from_packet(packet_histogram_t *histogram_packet)
  __attribute__((nonnull(1)))
  __attribute__((warn_unused_result))
  __attribute__((malloc));

void fhistogram_ref(fhistogram_t *histogram) __attribute__((nonnull(1)));
void fhistogram_unref(fhistogram_t *histogram) __attribute__((nonnull(1)));

/** @} */

#endif /* !FHISTOGRAM_H */
