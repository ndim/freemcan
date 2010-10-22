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


struct _packet_parser_t {
  unsigned int refs;
  packet_handler_value_table_t packet_handler_value_table;
  packet_handler_state_t     packet_handler_state;
  packet_handler_text_t      packet_handler_text;
  void *                     packet_handler_data;
};


packet_parser_t *packet_parser_new(packet_handler_value_table_t value_table_packet_handler,
                                   packet_handler_state_t state_packet_handler,
                                   packet_handler_text_t text_packet_handler,
                                   void *data)
{
  packet_parser_t *self = calloc(1, sizeof(packet_parser_t));
  assert(self);
  self->refs = 1;
  self->packet_handler_value_table = value_table_packet_handler;
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


void packet_parser_handle_frame(packet_parser_t *self, const frame_t *frame)
{
  switch (frame->type) {
  case FRAME_TYPE_STATE:
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
  case FRAME_TYPE_VALUE_TABLE:
    if (self->packet_handler_value_table) {
      const packet_value_table_header_t *header =
        (const packet_value_table_header_t *)&(frame->payload[0]);
      /* We need to do endianness conversion on all multi-byte values
       * in header, i.e. on header->duration. */
      const size_t vtab_size = frame->size - sizeof(*header);
      assert(vtab_size > 0);
      const size_t element_count = vtab_size/header->element_size;
      packet_value_table_t *vtab = packet_value_table_new(header->reason,
                                                          header->type,
                                                          time(NULL),
                                                          header->element_size,
                                                          element_count,
                                                          letoh16(header->duration),
                                                          letoh16(header->total_duration),
                                                          letoh16(header->total_table_size),
                                                          letoh32(header->token),
                                                          &(frame->payload[sizeof(*header)]));
      self->packet_handler_value_table(vtab, self->packet_handler_data);
      packet_value_table_unref(vtab);
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
