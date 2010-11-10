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
 * \ingroup firmware_generic
 *
 * Implement the frame-by-frame part of the communication protocl
 * (Layer 2).
 *
 * @{
 */


#include "global.h"
#include "uart-comm.h"
#include "frame-comm.h"


char magic_header[4] PROGMEM = FRAME_MAGIC_STR;


/** Start a data frame */
void frame_start(const frame_type_t frame_type,
                 const size_t payload_size)
{
  /* reset the checksum state */
  uart_send_checksum_reset();

  /* Send frame header magic value */
  uart_putb_P(&magic_header, sizeof(magic_header));

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
  uart_send_checksum();
}


/** Write a complete data frame */
void frame_send(const frame_type_t frame_type,
                const void *payload, const size_t payload_size)
{
  frame_start(frame_type, payload_size);
  uart_putb(payload, payload_size);
  frame_end();
}


void frame_send_P(const frame_type_t frame_type,
                  PGM_VOID_P payload, const size_t payload_size)
{
  frame_start(frame_type, payload_size);
  uart_putb_P(payload, payload_size);
  frame_end();
}

/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
