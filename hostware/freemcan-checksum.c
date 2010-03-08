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
 * \note The checksum engine uses a global state. Only use it for
 * one communication streams at a time.
 *
 * @{
 */


#include <assert.h>
#include <stdbool.h>
#include <unistd.h>


#include "frame-defs.h"
#include "freemcan-frame.h"
#include "freemcan-log.h"


/************************************************************************
 * Checksum
 ************************************************************************/


static uint16_t checksum_accu;


void checksum_reset()
{
  checksum_accu = 0x3e59;
}


bool checksum_match(const uint8_t value)
{
  const uint8_t checksum = (checksum_accu & 0xff);
  const bool retval = (checksum == value);
  return retval;
}


void checksum_write(const int fd)
{
  const uint8_t checksum = (checksum_accu & 0xff);
  write(fd, &checksum, sizeof(checksum));
}


void checksum_update(const uint8_t value)
{
  const uint8_t  n = (uint8_t)value;
  const uint16_t x = 8*n+2*n+n;
  const uint16_t r = (checksum_accu << 3) | (checksum_accu >> 13);
  const uint16_t v = r ^ x;
  checksum_accu = v;
}


/** @} */
