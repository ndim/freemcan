/** \file freemcan-packet.c
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
 */

#include "frame-defs.h"
#include "packet-defs.h"

#include "freemcan-log.h"
#include "freemcan-frame.h"
#include "freemcan-packet.h"


static packet_handler_histogram_t packet_handler_histogram = NULL;
static packet_handler_status_t    packet_handler_status = NULL;
static packet_handler_text_t      packet_handler_text = NULL;


static
void frame_handler(const frame_t *frame)
{
  switch (frame->type) {
  case FRAME_TYPE_STATUS:
    if (packet_handler_status) {
      packet_handler_status((const char *)frame->payload);
    }
    return;
  case FRAME_TYPE_TEXT:
    if (packet_handler_text) {
      packet_handler_text((const char *)frame->payload);
    }
    return;
  case FRAME_TYPE_HISTOGRAM:
    if (1) {
      const size_t hist_size = frame->size - 1;
      const uint8_t element_size = frame->payload[0];
      const size_t element_count = hist_size/element_size;
      packet_histogram_t hist;
      hist.type = '?'; /** \bug implement histogram types */
      hist.element_size = element_size;
      hist.element_count = element_count;
      hist.elements.e8 = &(frame->payload[1]);
      packet_handler_histogram(&hist);
    }
    return;
  }
  fmlog("Received frame of unknown type %c (%d=0x%x), size %d=0x%x",
	frame->type, frame->type, frame->type, frame->size, frame->size);
  fmlog_data((void *)frame->payload, frame->size);
}


void packet_reset_handlers()
{
  packet_handler_histogram = NULL;
  packet_handler_status = NULL;
  packet_handler_text = NULL;
  frame_reset_handler();
}


void packet_set_handlers(packet_handler_histogram_t histogram_packet_handler,
			 packet_handler_status_t status_packet_handler,
			 packet_handler_text_t text_packet_handler)
{
  packet_handler_histogram = histogram_packet_handler;
  packet_handler_status = status_packet_handler;
  packet_handler_text = text_packet_handler;
  frame_set_handler(frame_handler);
}
