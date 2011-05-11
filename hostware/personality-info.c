/** \file hostware/personality-info.c
 * \brief Personality Info data packets (layer 3) (implementation)
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
 * \defgroup freemcan_packets_personality_info Personality Info Data Packets
 * \ingroup freemcan_packets
 *
 * @{
 */

#include <assert.h>
#include <string.h>

#include "frame-defs.h"
#include "packet-defs.h"

#include "freemcan-log.h"
#include "frame.h"
#include "frame-parser.h"
#include "freemcan-packet.h"
#include "endian-conversion.h"


/** \todo Why don't we do the endianness conversion here? */
personality_info_t *personality_info_new(const uint16_t _sizeof_table,
                                         const uint8_t bits_per_value,
                                         const uint8_t units_per_second,
                                         const uint8_t param_data_size_timer_count,
                                         const uint8_t param_data_size_skip_samples,
                                         const uint16_t _personality_name_size,
                                         const char *personality_name)
{
  const size_t pn_size = letoh16(_personality_name_size);
  personality_info_t *result =
    malloc(sizeof(personality_info_t) + pn_size + 1);
  assert(result != NULL);

  result->refs         = 1;
  result->sizeof_table = letoh16(_sizeof_table);
  result->bits_per_value = bits_per_value;
  result->units_per_second = units_per_second;
  result->param_data_size_timer_count = param_data_size_timer_count;
  result->param_data_size_skip_samples = param_data_size_skip_samples;
  result->personality_name[0] = '\0';
  strncat(result->personality_name, personality_name, pn_size);

  return result;
}


void personality_info_ref(personality_info_t *pi)
{
  assert(pi->refs > 0);
  pi->refs++;
}


static
void personality_info_free(personality_info_t *pi)
{
  free(pi);
}


void personality_info_unref(personality_info_t *pi)
{
  assert(pi->refs > 0);
  pi->refs--;
  if (pi->refs == 0) {
    personality_info_free(pi);
  }
}


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
