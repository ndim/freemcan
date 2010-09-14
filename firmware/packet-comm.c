/** \file firmware/packet-comm.c
 * \brief ATmega packet communication interface (layer 3)
 *
 * \author Copyright (C) 2010 samplemaker
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
 * \defgroup firmware_comm Packet Communication
 * \ingroup firmware
 *
 * Implement packet part of the communication protocol (Layer 3).
 *
 * As all multi-byte values sent or received are little-endian, we can
 * just send and receive native values on the AVR and forget about
 * endianness altogether.
 *
 * @{
 */


#include <stdlib.h>
#include <string.h>

#include "global.h"
#include "frame-comm.h"
#include "packet-comm.h"


/** Send state message packet to host (layer 3).
 *
 * State messages are constant strings describing the FSM state we are
 * currently in.
 */
void send_state(const char *msg)
{
  const size_t len = strlen(msg);
  frame_send(FRAME_TYPE_STATE, msg, len);
}


/** Send text message packet to host (layer 3).
 *
 * If you need to send more than static text, use uprintf().
 */
void send_text(const char *msg)
{
  const size_t len = strlen(msg);
  frame_send(FRAME_TYPE_TEXT, msg, len);
}


/** @} */
