/** \file hostware/frame.c
 * \brief Data frame (layer 2) (implementation)
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
 * \defgroup freemcan_frame Data Frame
 * \ingroup hostware_generic
 *
 * @{
 */


#include <assert.h>
#include <stdbool.h>
#include <unistd.h>


#include "frame-defs.h"
#include "freemcan-checksum.h"
#include "frame.h"
#include "freemcan-log.h"



/************************************************************************
 * Frame reference counting/memory management
 ************************************************************************/


frame_t *frame_new(const size_t payload_size)
{
  frame_t *frame = malloc(sizeof(frame_t) + payload_size);
  assert(frame);
  frame->refs = 1;
  return frame;
}


void frame_ref(frame_t *self)
{
  assert(self->refs > 0);
  self->refs++;
}


void frame_unref(frame_t *self)
{
  assert(self->refs > 0);
  self->refs--;
  if (self->refs == 0) {
    free(self);
  }
}


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
