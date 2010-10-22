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


packet_value_table_t *packet_value_table_new(const packet_value_table_reason_t reason,
                                             const packet_value_table_type_t type,
                                             const time_t receive_time,
                                             const uint8_t element_size,
                                             const size_t element_count,
                                             const uint16_t duration,
                                             const uint16_t total_duration,
                                             const uint16_t total_table_size,
                                             const uint32_t token,
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
  result->duration          = duration;
  result->total_duration    = total_duration;
  result->total_table_size  = total_table_size;
  result->token             = token;

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
