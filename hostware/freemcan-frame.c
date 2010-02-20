/** \file freemcan-frame.c
 * \brief Data frame parser (implementation)
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


#include <assert.h>
#include <stdbool.h>


#include "frame-defs.h"
#include "freemcan-frame.h"
#include "freemcan-log.h"


/************************************************************************
 * Checksum
 ************************************************************************/


static uint16_t checksum_accu;


static
void checksum_reset()
{
  checksum_accu = 0x3e59;
}


static
bool checksum_match(const uint8_t test)
{
  const uint8_t checksum = (checksum_accu & 0xff);
  return (checksum == test);
}


static
void checksum_update(const uint8_t value)
{
  const uint8_t  n = (uint8_t)value;
  const uint16_t x = 8*n+2*n+n;
  const uint16_t r = (checksum_accu << 3) | (checksum_accu >> 13);
  const uint16_t v = r ^ x;
  checksum_accu = v;
}


/************************************************************************
 * Frame Handler (next layer)
 ************************************************************************/

static frame_handler_t frame_handler;
static void *frame_handler_data;


void frame_set_handler(frame_handler_t handler, void *data)
{
  frame_handler = handler;
  frame_handler_data = data;
}


void frame_reset_handler(void){
  frame_handler = NULL;
  frame_handler_data = NULL;
}



/************************************************************************
 * Frame Parser
 ************************************************************************/


typedef enum {
  STATE_MAGIC,
  STATE_SIZE,
  STATE_FRAME_TYPE,
  STATE_PAYLOAD,
  STATE_CHECKSUM
} state_t;

static state_t state = STATE_MAGIC;

static size_t offset = 0;

static const char *magic = "FMPK";

static uint16_t frame_size;
static uint8_t  frame_type;
static uint8_t  frame_checksum;

static frame_t  *frame_wip; /* work in progress */


static
void handle(const char ch)
{
  const uint8_t u = ch;

  /* update checksum even if we might reset it further down */
  checksum_update(u);

  fmlog("handle 0x%0x=%d", u, u);

  switch (state) {
  case STATE_MAGIC:
    fmlog("MAGIC");
    if (ch == magic[offset]) {
      if (offset == 0) {
	/* beginning of magic and header and frame */
	/* start new checksum */
	checksum_reset();
	checksum_update(u);
      }
      offset++;
      if (offset <= 3) {
	state = STATE_MAGIC;
	return;
      } else {
	offset = 0;
	state = STATE_SIZE;
	return;
      }
    } else { /* start looking for beginning of magic again */
      offset = 0;
      state = STATE_MAGIC;
      return;
    }
    break;
  case STATE_SIZE:
    fmlog("SIZE");
    switch (offset) {
    case 0:
      frame_size = (frame_size & 0xff00) | u;
      offset = 1;
      state = STATE_SIZE;
      return;
    case 1:
      frame_size = (frame_size & 0x00ff) | (((uint16_t)u)<<8);
      offset = 0;
      state = STATE_FRAME_TYPE;
      return;
    }
    break;
  case STATE_FRAME_TYPE:
    fmlog("TYPE");
    frame_type = u;
    offset = 0;
    frame_wip = malloc(sizeof(frame_t)+frame_size);
    assert(frame_wip);
    state = STATE_PAYLOAD;
    return;
  case STATE_PAYLOAD:
    fmlog("PAYLOAD");
    frame_wip->payload[offset] = u;
    offset++;
    if (offset < frame_size) {
      state = STATE_PAYLOAD;
      return;
    } else if (offset == frame_size) {
      offset = 0;
      state = STATE_CHECKSUM;
      return;
    }
    break;
  case STATE_CHECKSUM:
    fmlog("CHECKSUM");
    frame_checksum = u;
    if (checksum_match(frame_checksum)) {
      if (frame_handler) {
	frame_wip->type = frame_type;
	frame_wip->size = frame_size;
	frame_handler(frame_wip, frame_handler_data);
      }
      free(frame_wip);
      offset = 0;
      state = STATE_MAGIC;
      return;
    } else {
      offset = 0;
      state = STATE_MAGIC;
      return;
    }
    break;
  }
  fmlog("Illegal frame parser state.");
  abort();
}


void frame_parse(const void *buf, const size_t size)
{
  const char *cbuf = (const char *)buf;
  for (size_t i=0; i<size; i++) {
    handle(cbuf[i]);
  }
}
