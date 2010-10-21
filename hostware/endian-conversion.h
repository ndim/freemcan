/** \file hostware/endian-conversion.h
 * \brief Endianness conversion layer
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
 * \defgroup endian_conversion Endianness Conversion
 * \ingroup hostware_generic
 *
 */

#ifndef ENDIAN_CONVERSION_H
#define ENDIAN_CONVERSION_H

#include <stdint.h>

/** \addtogroup endian_conversion
 * \{
 */

#if defined(__i386__) || defined(__x86_64__)
# define ENDIANNESS_IS_LE
#elif defined(__ppc__)
# define ENDIANNESS_IS_BE
#else
# error Unsupported architecture.  Add your architecture to endian-conversion.h.
/* If running gcc, you can get an idea for a good macro to test by running
 *     $ gcc -dM -E -xc /dev/null
 */
#endif

#ifdef ENDIANNESS_IS_LE
static inline uint16_t letoh16(const uint16_t value)
{
  return value;
}
static inline uint16_t htole16(const uint16_t value)
{
  return value;
}
static inline uint32_t letoh32(const uint32_t value)
{
  return value;
}
static inline uint32_t htole32(const uint32_t value)
{
  return value;
}
#endif

#ifdef ENDIANNESS_IS_BE
/* to be implemented */
#endif

/** \} */

#endif /* !ENDIAN_CONVERSION_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
