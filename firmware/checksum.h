/** \file firmware/checksum.h
 * \brief Communication checksum interface
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
 * \addtogroup checksum
 * @{
 */


#ifndef CHECKSUM_H
#define CHECKSUM_H


#include <stdint.h>


typedef uint16_t checksum_accu_t;


/** Reset checksum state */
inline static
checksum_accu_t checksum_reset(void)
{
  return 0x3e59;
}


checksum_accu_t checksum_update(const checksum_accu_t accu, const uint8_t c);


/** Check whether data byte matches checksum */
inline static
uint8_t checksum_matches(const checksum_accu_t accu, const int8_t databyte)
{
  const uint8_t v = accu & 0xff;
  /* We store the difference in an 8bit variable, because if we just
   * do a "return (databyte == v);" the compiler will generate a 16bit
   * comparison for no good reason at all. The 8bit var d tricks the
   * compiler in doing an 8bit comparison. */
  const uint8_t d = v-databyte;
  return (0 == d);
}


#endif


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
