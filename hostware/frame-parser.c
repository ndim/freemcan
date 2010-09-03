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


/************************************************************************
 * Frame Parser
 ************************************************************************/


/** Static magic constant for comparision */
static const char *magic = FRAME_MAGIC_STR;

/** Parser state machine states */
typedef enum {
  /** waiting for magic number bytes to appear */
  STATE_MAGIC,
  /** waiting for frame size bytes */
  STATE_SIZE,
  /** waiting for frame type byte */
  STATE_FRAME_TYPE,
  /** waiting for payload bytes */
  STATE_PAYLOAD,
  /** waiting for checksum byte */
  STATE_CHECKSUM
} state_t;


/** Complete frame parser state */
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
  frame_parser_t *ps = calloc(1, sizeof(*ps));
  assert(ps);
  ps->refs = 1;
  ps->state = STATE_MAGIC;
  ps->checksum_input = checksum_new();
  ps->packet_parser = packet_parser;
  packet_parser_ref(ps->packet_parser);
  /* everything else initialized to 0 and NULL courtesy of calloc(3) */
  return ps;
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


/** step the parser FSM */
static
void step_fsm(frame_parser_t *ps, const char ch)
{
  const uint8_t u = ch;

  switch (ps->state) {
  case STATE_MAGIC:
    if (ch == magic[ps->offset]) {
      if (ps->offset == 0) {
        /* beginning of magic and header and frame */
        /* start new checksum */
        checksum_reset(ps->checksum_input);
      }
      checksum_update(ps->checksum_input, u);
      ps->offset++;
      if (ps->offset <= 3) {
        ps->state = STATE_MAGIC;
        return;
      } else {
        ps->offset = 0;
        ps->state = STATE_SIZE;
        return;
      }
    } else { /* start looking for beginning of magic again */
      ps->offset = 0;
      ps->state = STATE_MAGIC;
      return;
    }
    break;
  case STATE_SIZE:
    checksum_update(ps->checksum_input, u);
    switch (ps->offset) {
    case 0:
      ps->frame_size = (ps->frame_size & 0xff00) | u;
      ps->offset = 1;
      ps->state = STATE_SIZE;
      return;
    case 1:
      ps->frame_size = (ps->frame_size & 0x00ff) | (((uint16_t)u)<<8);
      ps->offset = 0;
      ps->state = STATE_FRAME_TYPE;
      return;
      /* We have the value of (offset) firmly under control from
       * within this very function, so we do not need to catch any
       * other cases. */
    }
    break;
  case STATE_FRAME_TYPE:
    checksum_update(ps->checksum_input, u);
    ps->frame_type = u;
    ps->offset = 0;
    /* Total size composed from:
     *  - size fixed parts of frame_t data structure
     *  - dynamic payload size
     *  - terminating convenience nul byte
     */
    ps->frame_wip = frame_new(ps->frame_size+1);
    assert(ps->frame_wip);
    ps->state = STATE_PAYLOAD;
    return;
  case STATE_PAYLOAD:
    checksum_update(ps->checksum_input, u);
    ps->frame_wip->payload[ps->offset] = u;
    ps->offset++;
    if (ps->offset < ps->frame_size) {
      ps->state = STATE_PAYLOAD;
      return;
    } else if (ps->offset == ps->frame_size) {
      ps->state = STATE_CHECKSUM;
      return;
    }
    break;
  case STATE_CHECKSUM:
    ps->frame_checksum = u;
    if (checksum_match(ps->checksum_input, ps->frame_checksum)) {
      if (ps->packet_parser) {
        /* nul-terminate the payload buffer for convenience */
        ps->frame_wip->payload[ps->offset] = '\0';
        ps->frame_wip->type = ps->frame_type;
        ps->frame_wip->size = ps->frame_size;
        if (enable_layer2_dump) {
          const frame_type_t type = ps->frame_wip->type;
          const uint16_t size     = ps->frame_wip->size;
          if ((32<=type) && (type<127)) {
            fmlog("Received type '%c'=0x%02x=%d frame with payload of size 0x%04x=%d",
                  type, type, type, size, size);
          } else {
            fmlog("Received type 0x%02x=%d frame with payload of size 0x%04x=%d",
                  type, type, size, size);
          }
          fmlog_data(ps->frame_wip->payload, size);
        }
        packet_parser_handle_frame(ps->packet_parser, ps->frame_wip);
      }
      frame_unref(ps->frame_wip);
      ps->offset = 0;
      ps->state = STATE_MAGIC;
      return;
    } else {
      ps->checksum_errors++;
      ps->offset = 0;
      ps->state = STATE_MAGIC;
      return;
    }
    break;
  /* No "default:" case on purpose: Let compiler complain about
   * unhandled values. */
  }
  fmlog("Illegal frame parser state.");
  abort();
}


/* documented in freemcan-frame.h */
bool enable_layer1_dump = false;


/* documented in freemcan-frame.h */
void frame_parser_bytes(frame_parser_t *self,
                        const void *buf, const size_t size)
{
  const char *cbuf = (const char *)buf;
  if (enable_layer1_dump) {
    fmlog("Received 0x%04x=%d bytes of layer 1 data", size, size);
    fmlog_data(buf, size);
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
