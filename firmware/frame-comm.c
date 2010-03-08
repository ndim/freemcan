/** \file firmware/frame-comm.c
 * \brief ATmega frame based communication implementation (layer 2)
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
 * \defgroup frame_comm Frame Communication (Layer 2)
 * \ingroup firmware
 * @{
 */


#include "uart-comm.h"
#include "frame-comm.h"

/** Start a data frame */
void frame_start(const frame_type_t frame_type,
		 const size_t payload_size)
{
  /* reset the checksum state */
  uart_checksum_reset();

  /* send frame header */
  const uint32_t header = FRAME_MAGIC;
  uart_putb(&header, sizeof(header));
  const uint16_t size = payload_size; /** \todo What about endianness? */
  uart_putb(&size, sizeof(size));
  const uint8_t type = frame_type;
  uart_putc((const char)type);
}


/** Finish sending a data frame */
void frame_end(void)
{
  /* send the final checksum */
  uart_checksum_send();
}


/** Write a complete data frame */
void frame_send(const frame_type_t frame_type,
		const void *payload, const size_t payload_size)
{
  frame_start(frame_type, payload_size);
  uart_putb(payload, payload_size);
  frame_end();
}

/** @} */
