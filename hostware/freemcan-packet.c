/** \file hostware/freemcan-packet.c
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
 * \defgroup freemcan_packet Data Packet Parser (Layer 3)
 * \ingroup hostware_generic
 *
 * @{
 */

#include <assert.h>
#include <string.h>

#include "frame-defs.h"
#include "packet-defs.h"

#include "freemcan-log.h"
#include "freemcan-frame.h"
#include "freemcan-packet.h"


packet_histogram_t *packet_histogram_new(const packet_histogram_type_t type,
					 const time_t receive_time,
					 const uint8_t element_size,
					 const size_t element_count,
					 const void *elements)
{
  packet_histogram_t *result = calloc(1, sizeof(packet_histogram_t));
  assert(result != NULL);
  result->refs = 1;

  const size_t sz = element_size*element_count;
  void *element_copy = malloc(sz);
  assert(element_copy);
  memcpy(element_copy, elements, sz);
  result->elements.ev   = element_copy;

  result->type          = type;
  result->receive_time  = receive_time;
  result->element_size  = element_size;
  result->element_count = element_count;

  return result;
}


void packet_histogram_ref(packet_histogram_t *hist_pack)
{
  assert(hist_pack->refs > 0);
  hist_pack->refs++;
}


static
void packet_histogram_free(packet_histogram_t *hist_pack)
{
  free(hist_pack->elements.ev);
  free(hist_pack);
}


void packet_histogram_unref(packet_histogram_t *hist_pack)
{
  assert(hist_pack->refs > 0);
  hist_pack->refs--;
  if (hist_pack->refs == 0) {
    packet_histogram_free(hist_pack);
  }
}


static packet_handler_histogram_t packet_handler_histogram = NULL;
static packet_handler_status_t    packet_handler_status = NULL;
static packet_handler_text_t      packet_handler_text = NULL;
static void *                     packet_handler_data = NULL;


static
void frame_handler(const frame_t *frame)
{
  switch (frame->type) {
  case FRAME_TYPE_STATUS:
    if (packet_handler_status) {
      packet_handler_status((const char *)frame->payload, packet_handler_data);
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
      const size_t hist_size = frame->size - sizeof(*header);
      const size_t element_count = hist_size/header->element_size;
      packet_histogram_t *hist = packet_histogram_new(header->type,
						      time(NULL),
						      header->element_size,
						      element_count,
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
  packet_handler_status = NULL;
  packet_handler_text = NULL;
  packet_handler_data = NULL;
  frame_reset_handler();
}


void packet_set_handlers(packet_handler_histogram_t histogram_packet_handler,
			 packet_handler_status_t status_packet_handler,
			 packet_handler_text_t text_packet_handler,
			 void *data)
{
  packet_handler_histogram = histogram_packet_handler;
  packet_handler_status = status_packet_handler;
  packet_handler_text = text_packet_handler;
  packet_handler_data = data;
  frame_set_handler(frame_handler);
}

/** @} */
