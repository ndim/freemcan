/** \file hostware/freemcan-checksum.c
 * \brief Checksum for layer 2 frames (implementation)
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
 * \defgroup freemcan_frame_checksum Data Frame Checksum (Layer 2)
 * \ingroup hostware_generic
 *
 * @{
 */


#include <assert.h>
#include <stdbool.h>
#include <unistd.h>


#include "frame-defs.h"
#include "freemcan-log.h"

#include "freemcan-checksum.h"



/************************************************************************
 * Checksum
 ************************************************************************/


/** Internals of opaque #checksum_t */
struct _checksum_t {
  unsigned int refs;
  uint16_t checksum_accu;
};


checksum_t *checksum_new(void)
{
  checksum_t *cs = malloc(sizeof(*cs));
  assert(cs);
  cs->refs = 1;
  checksum_reset(cs);
  return cs;
}


void checksum_ref(checksum_t *self)
{
  assert(self->refs > 0);
  self->refs++;
}


void checksum_unref(checksum_t *self)
{
  assert(self->refs > 0);
  self->refs--;
  if (self->refs == 0) {
    free(self);
  }
}


void checksum_reset(checksum_t *self)
{
  self->checksum_accu = 0x3e59;
}


bool checksum_match(checksum_t *self, const uint8_t value)
{
  const uint8_t checksum = (self->checksum_accu & 0xff);
  const bool retval = (checksum == value);
  return retval;
}


void checksum_write(checksum_t *self, const int fd)
{
  const uint8_t checksum = (self->checksum_accu & 0xff);
  const ssize_t sz = sizeof(checksum);
  assert(sz == write(fd, &checksum, sz));
}


uint8_t checksum_get(checksum_t *self)
{
  const uint8_t checksum = (self->checksum_accu & 0xff);
  return checksum;
}


void checksum_update(checksum_t *self, const uint8_t value)
{
  const uint8_t  n = (uint8_t)value;
  const uint16_t x = 8*n+2*n+n;
  const uint16_t r = (self->checksum_accu << 3) | (self->checksum_accu >> 13);
  const uint16_t v = r ^ x;
  self->checksum_accu = v;
}


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
