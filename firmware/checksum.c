/** \file firmware/checksum.c
 * \brief Communication checksum implementation
 *
 * \author Copyright (C) 2010 samplemaker
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
 * \defgroup checksum Communication checksum implementation
 * \ingroup firmware_generic
 *
 * Implements the checksum for the communication protocol.
 *
 * @{
 */


#include "checksum.h"


/** Update checksum
 *
 * \todo Use a good checksum algorithm with good values.
 *
 * We are calling this function twice - so not inlining the code saves
 * us some bytes that need to be programmed into the uC. For some
 * reason, gcc inlines the code anyway.
 */

checksum_accu_t checksum_update(const checksum_accu_t accu, const uint8_t data)
{
  const uint8_t  n = data;
  const uint16_t x = 8*n+2*n+n;
  const uint16_t r = (accu << 3) | (accu >> 13);
  const uint16_t v = r ^ x;
  return v;
}


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
