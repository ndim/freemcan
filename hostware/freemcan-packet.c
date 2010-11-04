/** \file hostware/freemcan-packet.c
 * \brief Data packets (layer 3) (implementation)
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
 * \defgroup freemcan_packets Data Packets
 * \ingroup hostware_generic
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
                                         const uint16_t _sizeof_value,
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
  result->sizeof_value = letoh16(_sizeof_value);
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


/** \todo Why don't we do the endianness conversion here? */
packet_value_table_t *packet_value_table_new(const packet_value_table_reason_t reason,
                                             const packet_value_table_type_t type,
                                             const time_t receive_time,
                                             const uint8_t element_size,
                                             const size_t element_count,
                                             const uint16_t _duration,
                                             const uint16_t _total_duration,
                                             const uint32_t _token,
                                             const void *elements)
{
  packet_value_table_t *result =
    malloc(sizeof(packet_value_table_t)+element_count*sizeof(uint32_t));
  assert(result != NULL);

  result->refs              = 1;
  result->reason            = reason;
  result->type              = type;
  result->receive_time      = receive_time;
  result->element_count     = element_count;
  result->orig_element_size = element_size;
  result->duration          = letoh16(_duration);
  result->total_duration    = letoh16(_total_duration);
  result->token             = letoh32(_token);

  if (!elements) {
    memset(result->elements, '\0', sizeof(result->elements[0])*element_count);
    return result;
  }

  const uint8_t *e8  = elements;

  switch (element_size) {
  case 1:
    for (size_t i=0; i<element_count; i++) {
      const uint32_t v = e8[i];
      result->elements[i] = v;
    }
    break;
  case 2:
    for (size_t i=0; i<element_count; i++) {
      const uint32_t v =
        (((uint32_t)e8[2*i+0]) << 0) +
        (((uint32_t)e8[2*i+1]) << 8);
      result->elements[i] = v;
    }
    break;
  case 3:
    for (size_t i=0; i<element_count; i++) {
      const uint32_t v =
        (((uint32_t)e8[3*i+0]) << 0) +
        (((uint32_t)e8[3*i+1]) << 8) +
        (((uint32_t)e8[3*i+2]) << 16);
      result->elements[i] = v;
    }
    break;
  case 4:
    for (size_t i=0; i<element_count; i++) {
      const uint32_t v =
        (((uint32_t)e8[4*i+0]) << 0) +
        (((uint32_t)e8[4*i+1]) << 8) +
        (((uint32_t)e8[4*i+2]) << 16) +
        (((uint32_t)e8[4*i+3]) << 24);
      result->elements[i] = v;
    }
    break;
  default:
    abort(); /* invalid value table element size */
    break;
  }

  return result;
}


void packet_value_table_ref(packet_value_table_t *value_table_packet)
{
  assert(value_table_packet->refs > 0);
  value_table_packet->refs++;
}


static
void packet_value_table_free(packet_value_table_t *value_table_packet)
{
  free(value_table_packet);
}


void packet_value_table_unref(packet_value_table_t *hist_pack)
{
  assert(hist_pack->refs > 0);
  hist_pack->refs--;
  if (hist_pack->refs == 0) {
    packet_value_table_free(hist_pack);
  }
}


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
