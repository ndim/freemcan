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
 * \defgroup frame_comm Frame Communication
 * \ingroup firmware
 *
 * Implement the frame-by-frame part of the communication protocl
 * (Layer 2).
 *
 * @{
 */


#include "global.h"
#include "uart-comm.h"
#include "frame-comm.h"

/** Start a data frame */
void frame_start(const frame_type_t frame_type,
                 const size_t payload_size)
{
  /* reset the checksum state */
  uart_checksum_reset();

  /* Send frame header magic value.  The uint32_t type avoids the
   * global .data space use a char array would cause. */
  const uint32_t header = FRAME_MAGIC_LE_U32;
  uart_putb(&header, sizeof(header));

  /* send payload size */
  const uint16_t size = payload_size;
  uart_putb(&size, sizeof(size));

  /* send frame type */
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


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
