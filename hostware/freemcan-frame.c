/** \file hostware/freemcan-frame.c
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
 * \defgroup freemcan_frame Data Frame Parser
 * \ingroup hostware_generic
 *
 * The only potential endianness issue in freemcan-frame.c is the
 * frame payload size in #frame_size which is read byte by byte in an
 * endianness independent fashion.
 *
 * @{
 */


#include <assert.h>
#include <stdbool.h>
#include <unistd.h>


#include "frame-defs.h"
#include "freemcan-checksum.h"
#include "freemcan-frame.h"
#include "freemcan-log.h"



/************************************************************************
 * Frame reference counting/memory management
 ************************************************************************/


static
frame_t *frame_new()
  __attribute__((malloc));

static
frame_t *frame_new(const size_t payload_size)
{
  frame_t *frame = malloc(sizeof(frame_t) + payload_size);
  assert(frame);
  frame->refs = 1;
  return frame;
}


void frame_ref(frame_t *frame)
{
  assert(frame->refs > 0);
  frame->refs++;
}


void frame_unref(frame_t *frame)
{
  assert(frame->refs > 0);
  frame->refs--;
  if (frame->refs == 0) {
    free(frame);
  }
}



/************************************************************************
 * Frame Handler (next layer)
 ************************************************************************/


static frame_handler_t frame_handler;


void frame_set_handler(frame_handler_t handler)
{
  frame_handler = handler;
}


void frame_reset_handler(void)
{
  frame_handler = NULL;
}



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
typedef struct {

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
} frame_parser_t;


static void parser_state_init(frame_parser_t *ps)
{
  ps->state = STATE_MAGIC;
  ps->offset = 0;
  ps->frame_wip = NULL;
  ps->checksum_errors = 0;
}


/** Global parser's complete state */
static frame_parser_t ps;

/** Initialize ps */
static void ff_init(void) __attribute__((constructor));
static void ff_init(void)
{
  parser_state_init(&ps);
}


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
	checksum_reset();
      }
      checksum_update(u);
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
    checksum_update(u);
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
    checksum_update(u);
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
    checksum_update(u);
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
    if (checksum_match(ps->frame_checksum)) {
      if (frame_handler) {
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
	frame_handler(ps->frame_wip);
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
void frame_parse_bytes(const void *buf, const size_t size)
{
  const char *cbuf = (const char *)buf;
  if (enable_layer1_dump) {
    fmlog("Received 0x%04x=%d bytes of layer 1 data", size, size);
    fmlog_data(buf, size);
  }
  for (size_t i=0; i<size; i++) {
    step_fsm(&ps, cbuf[i]);
  }
}

/** @} */
