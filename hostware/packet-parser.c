/** \file hostware/packet-parser.c
 * \brief Data packet parser (layer 3) (implementation)
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
 * \defgroup freemcan_packet_parser Data Packet Parser
 * \ingroup hostware_generic
 *
 * @{
 */

#include <assert.h>
#include <string.h>

#include "frame-defs.h"
#include "packet-defs.h"

#include "freemcan-log.h"
#include "frame.h"
#include "frame-parser.h"
#include "freemcan-packet.h"
#include "endian-conversion.h"


static packet_handler_histogram_t packet_handler_histogram = NULL;
static packet_handler_state_t     packet_handler_state = NULL;
static packet_handler_text_t      packet_handler_text = NULL;
static void *                     packet_handler_data = NULL;


static
void frame_handler(const frame_t *frame)
{
  switch (frame->type) {
  case FRAME_TYPE_STATE:
    if (packet_handler_state) {
      packet_handler_state((const char *)frame->payload, packet_handler_data);
    }
    return;
  case FRAME_TYPE_TEXT:
    if (packet_handler_text) {
      packet_handler_text((const char *)frame->payload, packet_handler_data);
    }
    return;
  case FRAME_TYPE_HISTOGRAM:
    if (packet_handler_histogram) {
      const packet_histogram_header_t *header =
	(const packet_histogram_header_t *)&(frame->payload[0]);
      /* We need to do endianness conversion on all multi-byte values
       * in header, i.e. on header->duration. */
      const size_t hist_size = frame->size - sizeof(*header);
      assert(hist_size > 0);
      const size_t element_count = hist_size/header->element_size;
      packet_histogram_t *hist = packet_histogram_new(header->type,
						      time(NULL),
						      header->element_size,
						      element_count,
						      letoh16(header->duration),
						      letoh16(header->total_duration),
						      &(frame->payload[sizeof(*header)]));
      packet_handler_histogram(hist, packet_handler_data);
      packet_histogram_unref(hist);
    }
    return;
  /* No "default:" case on purpose: Let compiler complain about
   * unhandled values. We are still prepared for uncaught values, but
   * after the switch() statement itself. (We might have received an
   * unhandled value from a remote system.) */
  }
  fmlog("Received frame of unknown type %c (%d=0x%x), size %d=0x%x",
	frame->type, frame->type, frame->type, frame->size, frame->size);
  fmlog_data((void *)frame->payload, frame->size);
}


void packet_reset_handlers()
{
  packet_handler_histogram = NULL;
  packet_handler_state = NULL;
  packet_handler_text = NULL;
  packet_handler_data = NULL;
  frame_parser_reset_handler(NULL); /** \bug HACK: Need to switch to non-global frame parser */
}


void packet_set_handlers(packet_handler_histogram_t histogram_packet_handler,
			 packet_handler_state_t state_packet_handler,
			 packet_handler_text_t text_packet_handler,
			 void *data)
{
  packet_handler_histogram = histogram_packet_handler;
  packet_handler_state = state_packet_handler;
  packet_handler_text = text_packet_handler;
  packet_handler_data = data;
  frame_parser_set_handler(NULL, frame_handler); /** \bug HACK: Need to switch to non-global frame parser */
}

/** @} */
