/** \file hostware/frame-parser.c
 * \brief Data frame parser (layer 2) (implementation)
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
 * \defgroup freemcan_frame_parser Data Frame Parser
 * \ingroup hostware_generic
 *
 * The frame parser (#frame_parser_t) is the code which composes
 * received bytes into complete frames. When a complete frame has been
 * received, it is handed to the next layer by calling
 * #packet_parser_handle_frame on it.
 *
 * The only potential endianness issue in hostware/frame-parser.c is
 * the frame payload size in #_frame_parser_t::frame_size which is read byte by byte in
 * an endianness independent fashion.
 *
 * @{
 */


#include <assert.h>
#include <stdbool.h>
#include <unistd.h>


#include "frame-defs.h"
#include "freemcan-checksum.h"
#include "frame-parser.h"
#include "freemcan-log.h"
#include "packet-parser.h"
#include "freemcan-tui.h"


/************************************************************************
 * Frame Parser
 ************************************************************************/


/** Static magic constant for comparision */
static const char *magic = FRAME_MAGIC_STR;

/** Parser state machine state
 *
 * Note that the states expecting bytes for multi-byte frame fields
 * always require considering the #_frame_parser_t::offset field as
 * well.
 */
typedef enum {
  /** waiting for magic number bytes to appear (together with offset) */
  STATE_MAGIC,
  /** waiting for frame size bytes (together with offset) */
  STATE_SIZE,
  /** waiting for frame type byte (1 byte only, no offset required) */
  STATE_FRAME_TYPE,
  /** waiting for payload bytes (together with offset) */
  STATE_PAYLOAD,
  /** waiting for checksum byte (1 byte only, no offset required) */
  STATE_CHECKSUM
} state_t;


/** Internals of opaque #frame_parser_t */
struct _frame_parser_t {
  /** Reference counter */
  unsigned int refs;

  /** Parser state machine state */
  state_t state;

  /** Parser state machine state data */
  size_t offset;

  /** The frame size for frame in progress */
  uint16_t frame_size;

  /** The frame type for frame in progress */
  uint8_t frame_type;

  /** The frame checksum for frame in progress */
  uint8_t frame_checksum;

  /** The parsed frame in progress */
  frame_t  *frame_wip; /* work in progress */

  /** Count the number of checksum errors we get */
  unsigned int checksum_errors;

  /** Packet parser */
  packet_parser_t *packet_parser;

  /** Checksum state */
  checksum_t *checksum_input;
};


frame_parser_t *frame_parser_new(packet_parser_t *packet_parser)
{
  assert(packet_parser);
  frame_parser_t *self = calloc(1, sizeof(*self));
  assert(self);
  self->refs = 1;
  self->state = STATE_MAGIC;
  self->checksum_input = checksum_new();
  self->packet_parser = packet_parser;
  packet_parser_ref(self->packet_parser);
  /* everything else initialized to 0 and NULL courtesy of calloc(3) */
  return self;
}


void frame_parser_ref(frame_parser_t *self)
{
  assert(self->refs > 0);
  self->refs++;
}


void frame_parser_unref(frame_parser_t *self)
{
  assert(self->refs > 0);
  self->refs--;
  if (self->refs == 0) {
    if (self->packet_parser) {
      packet_parser_unref(self->packet_parser);
    }
    checksum_unref(self->checksum_input);
    free(self);
  }
}


/************************************************************************
 * Frame Handler (next layer)
 ************************************************************************/


/* documented in freemcan-frame.h */
bool enable_layer2_dump = false;


/** Step the parser FSM */
static
void step_fsm(frame_parser_t *self, const char ch)
{
  const uint8_t u = ch;

  switch (self->state) {
  case STATE_MAGIC:
    if (ch == magic[self->offset]) {
      if (self->offset == 0) {
        /* beginning of magic and header and frame */
        /* start new checksum */
        checksum_reset(self->checksum_input);
      }
      checksum_update(self->checksum_input, u);
      self->offset++;
      if (self->offset <= 3) {
        self->state = STATE_MAGIC;
        return;
      } else {
        self->offset = 0;
        self->state = STATE_SIZE;
        return;
      }
    } else { /* start looking for beginning of magic again */
      self->offset = 0;
      self->state = STATE_MAGIC;
      return;
    }
    break;
  case STATE_SIZE:
    checksum_update(self->checksum_input, u);
    switch (self->offset) {
    case 0:
      self->frame_size = (self->frame_size & 0xff00) | u;
      self->offset = 1;
      self->state = STATE_SIZE;
      return;
    case 1:
      self->frame_size = (self->frame_size & 0x00ff) | (((uint16_t)u)<<8);
      self->offset = 0;
      self->state = STATE_FRAME_TYPE;
      return;
      /* We have the value of (offset) firmly under control from
       * within this very function, so we do not need to catch any
       * other cases. */
    }
    break;
  case STATE_FRAME_TYPE:
    checksum_update(self->checksum_input, u);
    self->frame_type = u;
    self->offset = 0;
    /* Total size composed from:
     *  - size fixed parts of frame_t data structure
     *  - dynamic payload size
     *  - terminating convenience nul byte
     */
    self->frame_wip = frame_new(self->frame_size+1);
    assert(self->frame_wip);
    self->state = STATE_PAYLOAD;
    return;
  case STATE_PAYLOAD:
    checksum_update(self->checksum_input, u);
    self->frame_wip->payload[self->offset] = u;
    self->offset++;
    if (self->offset < self->frame_size) {
      self->state = STATE_PAYLOAD;
      return;
    } else if (self->offset == self->frame_size) {
      self->state = STATE_CHECKSUM;
      return;
    }
    break;
  case STATE_CHECKSUM:
    self->frame_checksum = u;
    if (checksum_match(self->checksum_input, self->frame_checksum)) {
      if (self->packet_parser) {
        /* nul-terminate the payload buffer for convenience */
        self->frame_wip->payload[self->offset] = '\0';
        self->frame_wip->type = self->frame_type;
        self->frame_wip->size = self->frame_size;
        if (enable_layer2_dump) {
          const frame_type_t type = self->frame_wip->type;
          const uint16_t size     = self->frame_wip->size;
          if ((32<=type) && (type<127)) {
            fmlog("<Received type '%c'=0x%02x=%d frame with payload of size 0x%04x=%d",
                  type, type, type, size, size);
          } else {
            fmlog("<Received type 0x%02x=%d frame with payload of size 0x%04x=%d",
                  type, type, size, size);
          }
          fmlog_data("<<", self->frame_wip->payload, size);
        }
        update_last_received_size(self->frame_wip->size);
        packet_parser_handle_frame(self->packet_parser, self->frame_wip);
      }
      frame_unref(self->frame_wip);
      self->offset = 0;
      self->state = STATE_MAGIC;
      return;
    } else {
      self->checksum_errors++;
      self->offset = 0;
      self->state = STATE_MAGIC;
      return;
    }
    break;
  /* No "default:" case on purpose: Let compiler complain about
   * unhandled values. */
  }
  fmlog("Illegal frame parser state %d.", self->state);
  abort();
}


/* documented in freemcan-frame.h */
bool enable_layer1_dump = false;


/* documented in freemcan-frame.h */
void frame_parser_handle_bytes(frame_parser_t *self,
                               const void *buf, const size_t size)
{
  const char *cbuf = (const char *)buf;
  if (enable_layer1_dump) {
    fmlog("<Received 0x%04x=%d bytes of layer 1 data", size, size);
    fmlog_data("<<", buf, size);
  }
  for (size_t i=0; i<size; i++) {
    step_fsm(self, cbuf[i]);
  }
}


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
