/** \file firmware/frame-comm.h
 * \brief ATmega frame based communication interface (layer 2)
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
 * \addtogroup frame_comm
 * @{
 */

#ifndef FRAME_COMM_H
#define FRAME_COMM_H

#include <avr/pgmspace.h>

#include <stdlib.h>

#include "frame-defs.h"

void frame_send(const frame_type_t frame_type,
                const void *payload, const size_t payload_size);

void frame_send_P(const frame_type_t frame_type,
                  PGM_VOID_P payload, const size_t payload_size);

void frame_start(const frame_type_t frame_type,
                 const size_t payload_size);
void frame_end(void);

/** @} */

#endif /* !FRAME_COMM_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
