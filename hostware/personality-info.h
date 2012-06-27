/** \file hostware/freemcan-packet.h
 * \brief Data packets (layer 3) (interface)
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
 * \addtogroup freemcan_packets
 * @{
 */

#ifndef FREEMCAN_PACKET_PERSONALITY_INFO_H
#define FREEMCAN_PACKET_PERSONALITY_INFO_H

#include <time.h>

#include "packet-defs.h"


/** Parsed personality info */
typedef struct {
  int refs;
  size_t sizeof_table;
  size_t bits_per_value;
  unsigned int units_per_second;
  size_t param_data_size_timer_count;
  size_t param_data_size_skip_samples;
  char personality_name[];
} personality_info_t;


/** Create (allocate and initialize) a new personality_info_t instance.
 */
personality_info_t *personality_info_new(const uint16_t _sizeof_table,
                                         const uint8_t bits_per_value,
                                         const uint8_t units_per_second,
                                         const uint8_t param_data_size_timer_count,
                                         const uint8_t param_data_size_skip_samples,
                                         const uint16_t _personality_name_size,
                                         const char *personality_name)
  __attribute__(( warn_unused_result ))
  __attribute__(( nonnull(7) ))
  __attribute__(( malloc ));


/** Call this when you want to use value_table and store a pointer to it. */
void personality_info_ref(personality_info_t *pi)
  __attribute__(( nonnull(1) ));


/** Call this when you have finished using your pointer to value_table. */
void personality_info_unref(personality_info_t *pi)
  __attribute__(( nonnull(1) ));


/** Personality info from TUI (HACK) */
extern personality_info_t *personality_info;


/** @} */

#endif /* !FREEMCAN_PACKET_PERSONALITY_INFO_H */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
