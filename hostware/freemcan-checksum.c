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
  self->checksum_accu = 0xffff;
}


bool checksum_match(checksum_t *self, const uint16_t data)
{
  return (self->checksum_accu == data);
}


void checksum_write(checksum_t *self, const int fd)
{
  const ssize_t sz = sizeof(self->checksum_accu);
  assert(sz == write(fd, &self->checksum_accu, sz));
}


uint16_t checksum_get(checksum_t *self)
{
  return self->checksum_accu;
}


/**
 *
 * Adapted from avr-libc util/crc16.h
 */
static
uint16_t crc_ccitt_update(const uint16_t crc, const uint8_t data)
{
  const uint8_t data1 = data  ^ (crc & 0xff);
  const uint8_t data2 = data1 ^ (data1 << 4);
  return ((((uint16_t)data2 << 8) | ((crc >> 8) & 0xff))
          ^ (uint8_t)(data2 >> 4)
          ^ ((uint16_t)data2 << 3));
}


void checksum_update(checksum_t *self, const uint8_t value)
{
  self->checksum_accu = crc_ccitt_update(self->checksum_accu, value);
}


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
