/** \file hostware/packet-value-table.c
 * \brief Value Table data packets (layer 3) (implementation)
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
 * \defgroup freemcan_packets_value_table Value Table Data Packets
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

#include "personality-info.h"


/** Create new value table object in host conventions.
 *
 * Note that all multi-byte parameters which need endianness
 * conversion have a underscore prefix ("_duration") to make it
 * obvious that we should not use their values without doing
 * endianness conversion.
 */
packet_value_table_t *packet_value_table_new(const packet_value_table_reason_t reason,
                                             const packet_value_table_type_t type,
                                             const time_t receive_time,
                                             const uint8_t bits_per_value,
                                             const size_t element_count,
                                             const uint16_t _duration,
                                             const uint8_t param_buf_length,
                                             const void *data)
{
  packet_value_table_t *result =
    malloc(sizeof(packet_value_table_t)+element_count*sizeof(uint32_t));
  assert(result != NULL);

  result->refs              = 1;
  result->reason            = reason;
  result->type              = type;
  result->receive_time      = receive_time;
  result->element_count     = element_count;
  result->orig_bits_per_value = bits_per_value;
  result->duration          = letoh16(_duration);
  size_t ofs = 0;
  const char *cdata = (const char *)data;

  if (!personality_info) {
    fmlog_error("We have not received a personality_info packet yet. Aborting.");
    fmlog_error("Maybe the firmware interrupt load is too high so it drops "
                "the command frames?");
    abort();
  }

  /* read total_duration parameter from packet if present */
  if (ofs+2 < param_buf_length && personality_info->param_data_size_timer_count) {
    const uint16_t _total_duration = *((const uint16_t *)&cdata[ofs]);
    assert(2 == personality_info->param_data_size_timer_count);
    ofs += 2;
    result->total_duration    = letoh16(_total_duration);
  } else {
    result->total_duration    = -1;
  }

  /* read skip_samples parameter from packet if present */
  if (ofs+2 < param_buf_length && personality_info->param_data_size_skip_samples) {
    const uint16_t _skip_samples = *((const uint16_t *)&cdata[ofs]);
    assert(2 == personality_info->param_data_size_skip_samples);
    ofs += 2;
    result->skip_samples    = letoh16(_skip_samples);
  } else {
    result->skip_samples    = -1;
  }

  /* read token from packet if present */
  result->token = NULL;
  if (ofs < param_buf_length) {
    const size_t token_size = param_buf_length-ofs;
    if (token_size) {
      result->token = malloc(token_size);
      assert(result->token);
      memcpy(result->token, &cdata[ofs], token_size);
    }
  }

  const void *elements = (const void *)&cdata[param_buf_length];

  if (!elements) {
    memset(result->elements, '\0', sizeof(result->elements[0])*element_count);
    return result;
  }

  const uint8_t *e8  = elements;

  switch (bits_per_value) {
  case 8:
    for (size_t i=0; i<element_count; i++) {
      const uint32_t v = e8[i];
      result->elements[i] = v;
    }
    break;
  case 16:
    for (size_t i=0; i<element_count; i++) {
      const uint32_t v =
        (((uint32_t)e8[2*i+0]) << 0) +
        (((uint32_t)e8[2*i+1]) << 8);
      result->elements[i] = v;
    }
    break;
  case 24:
    for (size_t i=0; i<element_count; i++) {
      const uint32_t v =
        (((uint32_t)e8[3*i+0]) << 0) +
        (((uint32_t)e8[3*i+1]) << 8) +
        (((uint32_t)e8[3*i+2]) << 16);
      result->elements[i] = v;
    }
    break;
  case 32:
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
    fmlog("Fatal: Unhandled bits_per_value: %d\n", bits_per_value);
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
  if (value_table_packet->token) {
    free(value_table_packet->token);
  }
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
