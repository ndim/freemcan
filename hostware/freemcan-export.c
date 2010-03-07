/** \file hostware/freemcan-export.c
 * \brief histogram export functions
 *
 * \author Copyright (C) 2010 samplemaker
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
 * \defgroup freemcan_export Export Histogram Files
 * \ingroup hostware_generic
 * @{
 */

#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>

#include <time.h>

#include "freemcan-export.h"
#include "freemcan-log.h"


/* documented in freemcan-export.h */
void export_histogram(const packet_histogram_t *histogram_packet)
{
  const size_t element_count = histogram_packet->element_count;
  const size_t element_size = histogram_packet->element_size;
  const time_t t = time(NULL);
  const struct tm *tm_ = localtime(&t);
  assert(tm_);
  char type = 'X';
  switch (histogram_packet->type) {
  case PACKET_HISTOGRAM_DONE:
  case PACKET_HISTOGRAM_ABORTED:
  case PACKET_HISTOGRAM_INTERMEDIATE:
    type = histogram_packet->type;
    break;
  }
  char date[128];
  strftime(date, sizeof(date), "%Y-%m-%d.%H:%M:%S", tm_);
  char fname[256];
  snprintf(fname, sizeof(fname), "hist.%s.%c.dat", date, type);
  FILE *histfile = fopen(fname, "w");
  assert(histfile);
  fmlog("Writing histogram to file %s", fname);
  for (size_t i=0; i<element_count; i++) {
    uint32_t value;
    switch (element_size) {
    case 1: value = histogram_packet->elements.e8[i]; break;
    case 2: value = histogram_packet->elements.e16[i]; break;
    case 4: value = histogram_packet->elements.e32[i]; break;
    default: abort();
    }
    fprintf(histfile, "%d\t%u\n", i, value);
  }
  fclose(histfile);
}


/** @} */
