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

#include "packet-parser.h"

/** Structure for holding functional pointers for processing the completed packet
  *
  * Holding functional pointers for either processing histogram, state and text data either.
  * Holding also the data to be postprocessed
  *
  */
struct _packet_parser_t {
  unsigned int refs;
  packet_handler_histogram_t packet_handler_histogram;
  packet_handler_state_t     packet_handler_state;
  packet_handler_text_t      packet_handler_text;
  void *                     packet_handler_data;
};

/** Create object containing handler functions and data for processing
  *
  * This is a simple feed through to provide eihter a GUI for processing the frame
  * or a text user interface to process the data. Caller function will determine the parser
  * functions to processing the data.
  */
packet_parser_t *packet_parser_new(packet_handler_histogram_t histogram_packet_handler,
                                   packet_handler_state_t state_packet_handler,
                                   packet_handler_text_t text_packet_handler,
                                   void *data)
{
  packet_parser_t *self = calloc(1, sizeof(packet_parser_t));
  assert(self);
  self->refs = 1;
  self->packet_handler_histogram = histogram_packet_handler;
  self->packet_handler_state = state_packet_handler;
  self->packet_handler_text = text_packet_handler;
  self->packet_handler_data = data;
  /* everything else set to NULL by calloc */
  return self;
}


void packet_parser_ref(packet_parser_t *self)
{
  assert(self->refs > 0);
  self->refs++;
}


void packet_parser_unref(packet_parser_t *self)
{
  assert(self->refs > 0);
  self->refs--;
  if (self->refs == 0) {
    free(self);
  }
}

/** Data distribution to the handler functions respectively.
  *
  * This function is called if a frame is completed.
  *
  * Distribute the actual collected frame data to the handlerfunctions respectively according
  * to the three frame types existing (histogram, text for debugging and state frame).
  * Therefore the handlerfunctions are called directly from the packet_parser_t structure.
  */
void packet_parser_handle_frame(packet_parser_t *self, const frame_t *frame)
{
  switch (frame->type) {
  case FRAME_TYPE_STATE:
    /* if we have a handler for a state frame give the data to the handler function */
    if (self->packet_handler_state) {
      self->packet_handler_state((const char *)frame->payload,
                                 self->packet_handler_data);
    }
    return;
  case FRAME_TYPE_TEXT:
    if (self->packet_handler_text) {
      self->packet_handler_text((const char *)frame->payload,
                                self->packet_handler_data);
    }
    return;
  case FRAME_TYPE_HISTOGRAM:
    if (self->packet_handler_histogram) {
      /* Inside the frame payload we have a header field (element size in bytes, type, duration etc.)
         and a data field. The header of the frame is processed by a pointercast to
         packet_istogram_header_t                                               */
      const packet_histogram_header_t *header =
        (const packet_histogram_header_t *)&(frame->payload[0]);
      /* We need to do endianness conversion on all multi-byte values
       * in header, i.e. on header->duration. */
      /* The netto histogram data size is the payload size minus the header size */
      const size_t hist_size = frame->size - sizeof(*header);
      assert(hist_size > 0);
      const size_t element_count = hist_size/header->element_size;
      /* Create a new histogram data structure containing the histogram data and the header informations of
         the histogram  */
      packet_histogram_t *hist = packet_histogram_new(header->type,
                                                      time(NULL),
                                                      header->element_size,
                                                      element_count,
                                                      letoh16(header->duration),
                                                      letoh16(header->total_duration),
                                                      &(frame->payload[sizeof(*header)]));
      /* Call the handler function for processing the histogram structure, e.g. the tui packet_handler_histogram function */
      self->packet_handler_histogram(hist, self->packet_handler_data);
      /* The packet handler must have increased his reference counter to the historgram
         not to loose the data. However we do not need the histogram at this point any more and therefore we
         do an unref here */
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


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
